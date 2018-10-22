#' Plot a channel.
#'
#' @param abf an abf object.
#' @param channel channel to plot, channel id is 1-based.
#' @param colour wheter to plot in coloured mode.
#' @param time_unit time unit for the plot.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
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

#' Plot a channel with interval.
#'
#' @param abf an abf object.
#' @param intv an interval.
#' @param channel channel to plot, channel id is 1-based.
#' @param colour wheter to plot in coloured mode.
#' @param time_unit time unit for the plot.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PlotChannel_Intv <- function(abf, intv, channel = 1, colour = FALSE, time_unit = "tick", ...) {

  p <- PlotChannel(abf, channel, colour, time_unit, ...)
  #Convert intv to desired time_unit
  intv <- TickToTime(abf, time_unit, intv)
  p <- p + geom_vline(xintercept = intv[1:2], linetype = "dashed")

  return(p)
}

#' Plot a channel with cursors.
#'
#' @param abf an abf object.
#' @param cursor cursors to plot.
#' @param channel channel to plot, channel id is 1-based.
#' @param colour wheter to plot in coloured mode.
#' @param time_unit time unit for the plot.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PlotChannel_Cursor <- function(abf, cursor, channel = 1, colour = FALSE, time_unit = "tick", ...) {

  p <- PlotChannel(abf, channel, colour, time_unit, ...)
  #Convert cursors to desired time_unit
  cursor <- TickToTime(abf, time_unit, cursor)
  p <- p + geom_vline(xintercept = cursor, linetype = "dashed")

  return(p)
}

#' Fast plot a channel.
#'
#' @param abf an abf object.
#' @param channel channel to plot, channel id is 1-based.
#' @param colour wheter to plot in coloured mode.
#' @param time_unit time unit for the plot.
#' @param ratio sampling ratio.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PeekChannel <- function(abf, channel = 1, colour = FALSE, time_unit = "tick", ratio = 50, ...) {

  return(PlotChannel(abf, channel, colour, time_unit, sampling_ratio = ratio, ...))
}

#' Fast plot a channel with interval.
#'
#' @param abf an abf object.
#' @param intv an interval.
#' @param channel channel to plot, channel id is 1-based.
#' @param colour wheter to plot in coloured mode.
#' @param time_unit time unit for the plot.
#' @param ratio sampling ratio.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PeekChannel_Intv <- function(abf, intv, channel = 1, colour = FALSE, time_unit = "tick", ratio = 50, ...) {

  return(PlotChannel_Intv(abf, intv, channel, colour, time_unit, sampling_ratio = ratio, ...))
}

#' Fast plot a channel with cursors.
#'
#' @param abf an abf object.
#' @param cursor cursors to plot.
#' @param channel channel to plot, channel id is 1-based.
#' @param colour wheter to plot in coloured mode.
#' @param time_unit time unit for the plot.
#' @param ratio sampling ratio.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PeekChannel_Cursor <- function(abf, cursor, channel = 1, colour = FALSE, time_unit = "tick", ratio = 50, ...) {

  return(PlotChannel_Cursor(abf, cursor, channel, colour, time_unit, sampling_ratio = ratio, ...))
}

#' Plot all channels horizontally
#'
#' @param abf an abf object.
#' @param colour wheter to plot in coloured mode.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PlotAllChannel_H <- function(abf, colour = FALSE, ...) {

  p <- CollectAllChannel(abf, colour, ...)

  return(plot_grid(plotlist = p, nrow = 1))
}

#' Plot all channels vertically
#'
#' @param abf an abf object.
#' @param colour wheter to plot in coloured mode.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PlotAllChannel_V <- function(abf, colour = FALSE, ...) {

  p <- CollectAllChannel(abf, colour, ...)

  return(plot_grid(plotlist = p, ncol = 1))
}

#' Fast plot all channels horizontally
#'
#' @param abf an abf object.
#' @param colour wheter to plot in coloured mode.
#' @param ratio sampling ratio
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PeekAllChannel_H <- function(abf, colour = FALSE, ratio = 50, ...) {

  return(PlotAllChannel_H(abf, colour, sampling_ratio = ratio, ...))
}

#' Fast plot all channels vertically
#'
#' @param abf an abf object.
#' @param colour wheter to plot in coloured mode.
#' @param ratio sampling ratio
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PeekAllChannel_V <- function(abf, colour = FALSE, ratio = 50, ...) {

  return(PlotAllChannel_V(abf, colour, sampling_ratio = ratio, ...))
}

#' Plot all channels with interval horizontally
#'
#' @param abf an abf object.
#' @param intv an interval.
#' @param colour wheter to plot in coloured mode.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PlotAllChannel_Intv_H <- function(abf, intv, colour = FALSE, ...) {

  p <- CollectAllChannel_Intv(abf, intv, colour, ...)

  return(plot_grid(plotlist = p, nrow = 1))
}

#' Plot all channels with interval vertically
#'
#' @param abf an abf object.
#' @param intv an interval.
#' @param colour wheter to plot in coloured mode.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PlotAllChannel_Intv_V <- function(abf, intv, colour = FALSE, ...) {

  p <- CollectAllChannel_Intv(abf, intv, colour, ...)

  return(plot_grid(plotlist = p, ncol = 1))
}

#' Fast plot all channels with interval horizontally.
#'
#' @param abf an abf object.
#' @param intv an interval.
#' @param colour wheter to plot in coloured mode.
#' @param ratio sampling ratio.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PeekAllChannel_Intv_H <- function(abf, intv, colour = FALSE, ratio = 50, ...) {

  return(PlotAllChannel_Intv_H(abf, intv, colour, sampling_ratio = ratio, ...))
}

#' Fast plot all channels with interval vertically.
#'
#' @param abf an abf object.
#' @param intv an interval.
#' @param colour wheter to plot in coloured mode.
#' @param ratio sampling ratio.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PeekAllChannel_Intv_V <- function(abf, intv, colour = FALSE, ratio = 50, ...) {

  return(PlotAllChannel_Intv_V(abf, intv, colour, sampling_ratio = ratio, ...))
}

#' Plot all channels with cursors horizontally
#'
#' @param abf an abf object.
#' @param cursor cursors to plot.
#' @param colour wheter to plot in coloured mode.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PlotAllChannel_Cursor_H <- function(abf, cursor, colour = FALSE, ...) {

  p <- CollectAllChannel_Cursor(abf, cursor, colour, ...)

  return(plot_grid(plotlist = p, nrow = 1))
}

#' Plot all channels with cursors vertically.
#'
#' @param abf an abf object.
#' @param cursor cursors to plot.
#' @param colour wheter to plot in coloured mode.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PlotAllChannel_Cursor_V <- function(abf, cursor, colour = FALSE, ...) {

  p <- CollectAllChannel_Cursor(abf, cursor, colour, ...)

  return(plot_grid(plotlist = p, ncol = 1))
}

#' Fast all channels with cursors horizontally.
#'
#' @param abf an abf object.
#' @param cursor cursors to plot.
#' @param colour wheter to plot in coloured mode.
#' @param ratio sampling ratio.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PeekAllChannel_Cursor_H <- function(abf, cursor, colour = FALSE, ratio = 50, ...) {

  return(PlotAllChannel_Cursor_H(abf, cursor, colour, sampling_ratio = ratio, ...))
}

#' Fast all channels with cursors vertically.
#'
#' @param abf an abf object.
#' @param cursor cursors to plot.
#' @param colour wheter to plot in coloured mode.
#' @param ratio sampling ratio.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PeekAllChannel_Cursor_V <- function(abf, cursor, colour = FALSE, ratio = 50, ...) {

  return(PlotAllChannel_Cursor_V(abf, cursor, colour, sampling_ratio = ratio, ...))
}
