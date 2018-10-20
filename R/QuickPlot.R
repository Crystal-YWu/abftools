#' Title
#'
#' @param abf
#' @param pos
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param abf
#' @param channel
#' @param episodeX
#' @param episodeY
#' @param intv
#'
#' @return
#' @export
#'
#' @examples
QuickPlotTrace <- function(abf, channel, episodeX, episodeY, intv) {

  ch <- abf[[channel]]
  mask <- intv[1]:intv[2]
  traceX <- ch[mask, episodeX]
  traceY <- ch[mask, episodeY]

  qplot(x = traceX, y = traceY, geom = "point") + theme_bw()
}
