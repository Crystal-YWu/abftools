GetChannelP <- function(abf, channel, ...) {

  df <- melt(abf, channel, ...)
  chan_desc <- attr(abf, "ChannelDesc")[channel]

  p <- ggplot(df, aes_string("time", chan_desc)) + theme_bw()

  return(p)
}
#' Title
#'
#' @param abf
#' @param channel
#' @param colour
#' @param time_unit
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PlotChannel <- function(abf, channel = 1, colour = FALSE, time_unit = "tick", ...) {

  p <- GetChannelP(abf, channel, time_unit = time_unit, ...)

  if (colour) {
    p <- p + geom_line(aes_string(colour = "Episode"))
  } else {
    p <- p + geom_line(aes_string(group = "Episode"))
  }

  ydesc <- attr(abf, "ChannelDesc")[channel]
  yunit <- attr(abf, "ChannelUnit")[channel]
  ylabel <- paste0(ydesc, " (", yunit, ")")
  xdesc <- "Time"
  xunit <- time_unit
  xlabel <- paste0(xdesc, " (", xunit, ")")
  p <- p + xlab(xlabel) + ylab(ylabel)

  return(p)
}
#' Title
#'
#' @param abf
#' @param intv
#' @param channel
#' @param colour
#' @param time_unit
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PlotChannel_Intv <- function(abf, intv, channel = 1, colour = FALSE, time_unit = "tick", ...) {

  p <- PlotChannel(abf, channel, colour, time_unit, ...)
  #Convert intv to desired time_unit
  intv <- TickToTime(abf, time_unit, intv)
  p <- p + geom_vline(xintercept = intv[1:2], linetype = "dashed")

  return(p)
}
#' Title
#'
#' @param abf
#' @param channel
#' @param colour
#' @param ratio
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PeekChannel <- function(abf, channel = 1, colour = FALSE, ratio = 50, ...) {

  return(PlotChannel(abf, channel, colour, sampling_ratio = ratio, ...))
}
#' Title
#'
#' @param abf
#' @param intv
#' @param channel
#' @param colour
#' @param ratio
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PeekChannel_Intv <- function(abf, intv, channel = 1, colour = FALSE, ratio = 50, ...) {

  return(PlotChannel_Intv(abf, intv, channel, colour, sampling_ratio = ratio, ...))
}

CollectAllChannel <- function(abf, colour, ...) {

  p <- list()
  n <- nChan(abf)

  for (i in seq(n)) {
    p[[i]] <- PlotChannel(abf, i, colour, ...)
  }

  return(p)
}
#' Title
#'
#' @param abf
#' @param colour
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PlotAllChannel_H <- function(abf, colour = FALSE, ...) {

  p <- CollectAllChannel(abf, colour, ...)

  return(plot_grid(plotlist = p, nrow = 1))
}
#' Title
#'
#' @param abf
#' @param colour
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PlotAllChannel_V <- function(abf, colour = FALSE, ...) {

  p <- CollectAllChannel(abf, colour, ...)

  return(plot_grid(plotlist = p, ncol = 1))
}
#' Title
#'
#' @param abf
#' @param colour
#' @param ratio
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PeekAllChannel_H <- function(abf, colour = FALSE, ratio = 50, ...) {

  return(PlotAllChannel_H(abf, colour, sampling_ratio = ratio, ...))
}
#' Title
#'
#' @param abf
#' @param colour
#' @param ratio
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PeekAllChannel_V <- function(abf, colour = FALSE, ratio = 50, ...) {

  return(PlotAllChannel_V(abf, colour, sampling_ratio = ratio, ...))
}

CollectAllChannel_Intv <- function(abf, intv, colour, ...) {

  p <- list()
  n <- nChan(abf)

  for (i in seq(n)) {
    p[[i]] <- PlotChannel_Intv(abf, intv, i, colour, ...)
  }

  return(p)
}
#' Title
#'
#' @param abf
#' @param intv
#' @param colour
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PlotAllChannel_Intv_H <- function(abf, intv, colour = FALSE, ...) {

  p <- CollectAllChannel_Intv(abf, intv, colour, ...)

  return(plot_grid(plotlist = p, nrow = 1))
}
#' Title
#'
#' @param abf
#' @param intv
#' @param colour
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PlotAllChannel_Intv_V <- function(abf, intv, colour = FALSE, ...) {

  p <- CollectAllChannel_Intv(abf, intv, colour, ...)

  return(plot_grid(plotlist = p, ncol = 1))
}
#' Title
#'
#' @param abf
#' @param intv
#' @param colour
#' @param ratio
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PeekAllChannel_Intv_H <- function(abf, intv, colour = FALSE, ratio = 50, ...) {

  return(PlotAllChannel_Intv_H(abf, intv, colour, sampling_ratio = ratio, ...))
}
#' Title
#'
#' @param abf
#' @param intv
#' @param colour
#' @param ratio
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PeekAllChannel_Intv_V <- function(abf, intv, colour = FALSE, ratio = 50, ...) {

  return(PlotAllChannel_Intv_V(abf, intv, colour, sampling_ratio = ratio, ...))
}