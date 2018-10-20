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

  ydesc <- GetChannelDesc(abf)[channel]
  yunit <- GetChannelUnit(abf)[channel]
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
#' @param cursor
#' @param channel
#' @param colour
#' @param time_unit
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PlotChannel_Cursor <- function(abf, cursor, channel = 1, colour = FALSE, time_unit = "tick", ...) {

  p <- PlotChannel(abf, channel, colour, time_unit, ...)
  #Convert cursors to desired time_unit
  cursor <- TickToTime(abf, time_unit, cursor)
  p <- p + geom_vline(xintercept = cursor, linetype = "dashed")

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

#' Title
#'
#' @param abf
#' @param cursor
#' @param channel
#' @param colour
#' @param ratio
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PeekChannel_Cursor <- function(abf, cursor, channel = 1, colour = FALSE, ratio = 50, ...) {

  return(PlotChannel_Cursor(abf, cursor, channel, colour, sampling_ratio = ratio, ...))
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

#' Title
#'
#' @param abf
#' @param cursor
#' @param colour
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PlotAllChannel_Cursor_H <- function(abf, cursor, colour = FALSE, ...) {

  p <- CollectAllChannel_Cursor(abf, cursor, colour, ...)

  return(plot_grid(plotlist = p, nrow = 1))
}

#' Title
#'
#' @param abf
#' @param cursor
#' @param colour
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PlotAllChannel_Cursor_V <- function(abf, cursor, colour = FALSE, ...) {

  p <- CollectAllChannel_Cursor(abf, cursor, colour, ...)

  return(plot_grid(plotlist = p, ncol = 1))
}

#' Title
#'
#' @param abf
#' @param cursor
#' @param colour
#' @param ratio
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PeekAllChannel_Cursor_H <- function(abf, cursor, colour = FALSE, ratio = 50, ...) {

  return(PlotAllChannel_Cursor_H(abf, cursor, colour, sampling_ratio = ratio, ...))
}

#' Title
#'
#' @param abf
#' @param cursor
#' @param colour
#' @param ratio
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
PeekAllChannel_Cursor_V <- function(abf, cursor, colour = FALSE, ratio = 50, ...) {

  return(PlotAllChannel_Cursor_V(abf, cursor, colour, sampling_ratio = ratio, ...))
}
