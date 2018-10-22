#' Quick plot I-V curves at given position
#'
#' @param abf an abf object.
#' @param pos an interval or a cursor.
#'
#' @return a ggplot object.
#' @export
#'
QuickPlotIV <- function(abf, pos, smooth = FALSE) {

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

  geom_type <- ifelse(smooth, "smooth", "line")
  qplot(x = voltage, y = current, geom = geom_type) + theme_bw()
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

  chX <- abf[[channelX]]
  chY <- abf[[channelY]]
  mask <- intv[1]:intv[2]
  traceX <- chX[mask, episodeX]
  traceY <- chY[mask, episodeY]

  qplot(x = traceX, y = traceY, geom = "point") + theme_bw()
}
