#' Quick plot I-V curves at given position
#'
#' @param abf an abf or a list of abf objects.
#' @param pos an interval/cursor or a list of intervals/cursors.
#' @param colour whether to plot in coloured mode.
#'
#' @return a ggplot object.
#' @export
#'
QuickPlotIV <- function(abf, pos, colour = FALSE) {

  if (IsAbf(abf)) {

    current_channel <- GetFirstCurrentChan(abf)
    voltage_channel <- GetFirstVoltageChan(abf)
    if (length(pos) == 1) {
      #a cursor
      current <- abf[pos, , current_channel]
      voltage <- abf[pos, , voltage_channel]
    } else {
      mask <- pos[1]:pos[2]
      current <- colMeans(abf[[current_channel]][mask, ])
      voltage <- colMeans(abf[[voltage_channel]][mask, ])
    }

    return(qplot(x = voltage, y = current, geom = "line") + theme_bw())
  } else if (IsAbfList(abf)) {

    melted <- MeltAbfMean(abf, pos)

    cname <- colnames(melted)
    xcol <- as.name(FirstElement(cname[startsWith(cname, "Voltage")]))
    ycol <- as.name(FirstElement(cname[startsWith(cname, "Current")]))
    p <- ggplot(melted, aes_string(x = xcol, y = ycol)) + theme_bw()
    if (colour) {
      p <- p + geom_line(aes_string(colour = "id"))
    } else {
      p <- p + geom_line(aes_string(group = "id"))
    }
    #Get rid of ``
    p <- p + xlab(as.character(xcol)) + ylab(as.character(ycol))

    return(p)
  } else {
    err_class_abf_list()
  }

}

#' Quick plot trace vs. trace curve.
#'
#' @param abf an abf object.
#' @param channelX channel of X trace.
#' @param episodeX episode of X trace.
#' @param channelY channel of Y trace.
#' @param episodeY channel of Y trace.
#' @param intv interval of the traces to be plotted.
#'
#' @return a ggplot object.
#' @export
#'
QuickPlotTrace <- function(abf, channelX, episodeX, channelY, episodeY, intv) {

  if (!IsAbf(abf)) {
    err_class_abf()
  }
  if (!AssertChannel(abf, channelX) || !AssertChannel(abf, channelY)) {
    err_channel()
  }
  if (!AssertEpisode(abf, episodeX) || !AssertEpisode(abf, episodeY)) {
    err_epi()
  }

  chX <- abf[[channelX]]
  chY <- abf[[channelY]]
  mask <- intv[1]:intv[2]
  traceX <- chX[mask, episodeX]
  traceY <- chY[mask, episodeY]

  qplot(x = traceX, y = traceY, geom = "point") + theme_bw()
}
