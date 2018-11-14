#' Plot a channel of an abf object.
#'
#' @param abf an abf object.
#' @param intv OPTIONAL, an interval (a vector of c(start, end, len)) to plot on top.
#' @param cursor OPTIONAL, cursors (a vector of positions) to plot on top.
#' @param channel channel to plot, 1-based.
#' @param colour whether to plot in coloured mode.
#' @param time_unit time unit for x axis.
#' @param auto_zoom whether to zoom in the plot automatically.
#' @param title OPTIONAL, title of the plot.
#' @param ... other arguments passed to melt, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PlotChannel <- function(abf, intv = NULL, cursor = NULL, channel = 1L,
                        colour = FALSE, time_unit = "tick", auto_zoom = FALSE,
                        title = NULL, ...) {

  if (!IsAbf(abf)) {
    err_class_abf()
  }
  if (!AssertChannel(abf, channel)) {
    err_channel()
  }

  p <- CollectCh(abf, intv = intv, curs = cursor, channel = channel,
                 colour = colour, time_unit = time_unit, auto_zoom = auto_zoom,
                 ...)
  if (!is.null(title)) {
    p <- p + ggtitle(as.character(title))
  }

  return(p)
}

#' Plot a channel with an interval.
#'
#' @param abf an abf object.
#' @param intv an interval (a vector of c(start, end, len)) to plot on top.
#' @param channel channel to plot, 1-based.
#' @param colour whether to plot in coloured mode.
#' @param time_unit time unit for x axis.
#' @param auto_zoom whether to zoom in the plot automatically.
#' @param title OPTIONAL, title of the plot.
#' @param ... other arguments passed to melt, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PlotChannel_Intv <- function(abf, intv, channel = 1L, colour = FALSE,
                             time_unit = "tick", auto_zoom = FALSE,
                             title = NULL, ...) {

  p <- PlotChannel(abf, intv = intv, cursor = NULL, channel = channel,
                   colour = colour, time_unit = time_unit, auto_zoom = auto_zoom,
                   title = title, ...)

  return(p)
}

#' Plot a channel with cursors.
#'
#' @param abf an abf object.
#' @param cursor cursors (a vector of positions) to plot on top.
#' @param channel channel to plot, 1-based.
#' @param colour whether to plot in coloured mode.
#' @param time_unit time unit for x axis.
#' @param auto_zoom whether to zoom in the plot automatically.
#' @param title OPTIONAL, title of the plot.
#' @param ... other arguments passed to melt, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PlotChannel_Cursor <- function(abf, cursor, channel = 1L, colour = FALSE,
                               time_unit = "tick", auto_zoom = FALSE,
                               title = NULL, ...) {

  p <- PlotChannel(abf, intv = NULL, cursor = cursor, channel = channel,
                   colour = colour, time_unit = time_unit, auto_zoom = auto_zoom,
                   title = title, ...)

  return(p)
}

#' Fast plot a channel.
#'
#' @param abf an abf object.
#' @param intv OPTIONAL, an interval (a vector of c(start, end, len)) to plot on top.
#' @param cursor OPTIONAL, cursors (a vector of positions) to plot on top.
#' @param channel channel to plot, 1-based.
#' @param colour whether to plot in coloured mode.
#' @param time_unit time unit for x axis.
#' @param auto_zoom whether to zoom in the plot automatically.
#' @param title OPTIONAL, title of the plot.
#' @param ratio OPTIONAL, sampling ratio
#' @param ... other arguments passed to melt, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PeekChannel <- function(abf, intv = NULL, cursor = NULL, channel = 1L,
                        colour = FALSE, time_unit = "tick", auto_zoom = FALSE,
                        title = NULL, ratio = 50L, ...) {

  p <- PlotChannel(abf, intv = intv, cursor = cursor, channel = channel,
                   colour = colour, time_unit = time_unit, auto_zoom = auto_zoom,
                   title = title, sampling_ratio = ratio, ...)

  return(p)
}

#' Fast plot a channel with interval.
#'
#' @param abf an abf object.
#' @param intv an interval (a vector of c(start, end, len)) to plot on top.
#' @param channel channel to plot, 1-based.
#' @param colour whether to plot in coloured mode.
#' @param time_unit time unit for x axis.
#' @param auto_zoom whether to zoom in the plot automatically.
#' @param title OPTIONAL, title of the plot.
#' @param ratio OPTIONAL, sampling ratio
#' @param ... other arguments passed to melt, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PeekChannel_Intv <- function(abf, intv, channel = 1L, colour = FALSE,
                             time_unit = "tick", auto_zoom = FALSE,
                             title = NULL, ratio = 50L, ...) {

  p <- PlotChannel(abf, intv = intv, cursor = NULL, channel = channel,
                   colour = colour, time_unit = time_unit, auto_zoom = auto_zoom,
                   title = title, sampling_ratio = ratio, ...)

  return(p)
}

#' Fast plot a channel with cursors.
#'
#' @param abf an abf object.
#' @param cursor cursors (a vector of positions) to plot on top.
#' @param channel channel to plot, 1-based.
#' @param colour whether to plot in coloured mode.
#' @param time_unit time unit for x axis.
#' @param auto_zoom whether to zoom in the plot automatically.
#' @param title OPTIONAL, title of the plot.
#' @param ratio OPTIONAL, sampling ratio
#' @param ... other arguments passed to melt, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PeekChannel_Cursor <- function(abf, cursor, channel = 1L, colour = FALSE,
                               time_unit = "tick", auto_zoom = FALSE,
                               title = NULL, ratio = 50L, ...) {

  p <- PlotChannel(abf, intv = NULL, cursor = cursor, channel = channel,
                   colour = colour, time_unit = time_unit, auto_zoom = auto_zoom,
                   title = title, sampling_ratio = ratio, ...)

  return(p)
}

#' Plot all channels of an abf object.
#'
#' @param abf an abf object.
#' @param intv OPTIONAL, an interval (a vector of c(start, end, len)) to plot on top.
#' @param cursor OPTIONAL, cursors (a vector of positions) to plot on top.
#' @param colour whether to plot in coloured mode.
#' @param time_unit time unit for x axis.
#' @param auto_zoom whether to zoom in the plot automatically.
#' @param title OPTIONAL, title of the plot.
#' @param arrange arrangement of the subplots, can be "H" (horizontal), "V" (vertical)
#' @param ... other arguments passed to melt, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PlotAllChannel <- function(abf, intv = NULL, cursor = NULL, colour = FALSE,
                           time_unit = "tick", auto_zoom = FALSE, title = NULL,
                           arrange = "H", ...) {

  if (!IsAbf(abf)) {
    err_class_abf()
  }

  p <- list()
  n <- nChan(abf)
  for (i in seq_len(n)) {
    p[[i]] <- CollectCh(abf, intv = intv, curs = cursor, channel = i,
                        colour = colour, time_unit = time_unit, auto_zoom = auto_zoom,
                        ...)
  }
  if (!is.null(title)) {
    p[[1]] <- p[[1]] + ggtitle(as.character(title))
  }

  if (startsWith(toupper(arrange), "H")) {
    pg <- plot_grid(plotlist = p, nrow = 1, align = "h")
  } else {
    pg <- plot_grid(plotlist = p, ncol = 1, align = "v")
  }

  return(pg)
}

#' Fastp lot all channels of an abf object.
#'
#' @param abf an abf object.
#' @param intv OPTIONAL, an interval (a vector of c(start, end, len)) to plot on top.
#' @param cursor OPTIONAL, cursors (a vector of positions) to plot on top.
#' @param colour whether to plot in coloured mode.
#' @param time_unit time unit for x axis.
#' @param auto_zoom whether to zoom in the plot automatically.
#' @param title OPTIONAL, title of the plot.
#' @param arrange arrangement of the subplots, can be "H" (horizontal), "V" (vertical)
#' @param ratio OPTIONAL, sampling ratio
#' @param ... other arguments passed to melt, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
PeekAllChannel <- function(abf, intv = NULL, cursor = NULL, colour = FALSE,
                           time_unit = "tick", auto_zoom = FALSE, title = NULL,
                           arrange = "H", ratio = 50L, ...) {

  p <- PlotAllChannel(abf, intv = intv, cursor = cursor, colour = colour,
                      time_unit = time_unit, auto_zoom = auto_zoom, title = title,
                      arrange = arrange, sampling_ratio = ratio, ...)

  return(p)
}
