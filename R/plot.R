#' Plot channel data of abf objects.
#'
#' @description AbfPlotChannel() is a unified wrapper to abf_plot_td(), it is
#' capable of plotting multiple abf object facetted by id, multiple channels
#' facetted by channel.
#'
#' When printing the returned plot with a large number of abf objects, it can be
#' very slow. It is recommended to use AbfPeekChannel() for previewing and plot
#' tuning.
#'
#' @param abf an abf object/a list of abf objects
#' @param intv an INDEX intv to sample abf.
#' @param channel channels to plot.
#' @param episode episodes to group.
#' @param concat_epi wheter to concatenate all episodes.
#' @param cursor add indicative cursors to plot.
#' @param time_unit time unit for X axis.
#' @param colour wheter to plot in coloured mode.
#' @param auto_zoom wheter to zoom into area focused by cursor.
#' @param title add title to the plot.
#' @param ... passed to MeltAbf().
#'
#' @return a ggplot object.
#' @export
#'
AbfPlotChannel <- function(abf, intv = NULL, channel = GetAllChannels(abf),
                           episode = GetAvailEpisodes(abf), concat_epi = FALSE,
                           cursor = NULL, time_unit = "tick", colour = max(nEpi(abf)) > 1L,
                           auto_zoom = FALSE, title = NULL, ...) {

  CheckArgs(abf, chan = channel, allow_list = TRUE)

  p <- abf_plot_td(abf = abf, intv = intv, channel = channel, episode = episode,
                   concat_epi = concat_epi, time_unit = time_unit, colour = colour, ...)


  if (!is.null(cursor)) {
    p <- ApplyCursor(p, cursor_x = cursor)
    if (auto_zoom) {
        p <- ApplyForceZoomY(p, xrange = cursor, clipping = length(channel) > 1)
    }
  }

  #Axes labels and facet
  xlabel <- GetAxisLabel("Time", time_unit, style = "%s (%s)")
  if (IsAbfList(abf)) {
    if (length(channel) > 1L) {
      p <- p + facet_grid(stats::reformulate("id", "Channel"),
                          labeller = DefaultLabeller(abf[[1]]),
                          scales = "free_y")
      ylabel <- "Channel"
    } else {
      p <- p + facet_wrap(stats::reformulate("id"),
                          labeller = DefaultLabeller(abf[[1]]),
                          scales = "fixed")
      ylabel <- DefaultChanLabel(abf[[1]])[channel]
    }
  } else {
    if (length(channel) > 1L) {
      p <- p + facet_grid(stats::reformulate(".", "Channel"),
                          labeller = DefaultLabeller(abf),
                          scales = "free_y")
      ylabel <- "Channel"
    } else {
      ylabel <- DefaultChanLabel(abf)[channel]
    }
  }

  if (!is.null(title)) {
    p <- p + labs(x = xlabel, y = ylabel, title = title)
  } else {
    p <- p + labs(x = xlabel, y = ylabel)
  }

  p + geom_line()
}

#' @rdname AbfPlotChannel
#' @export
#'
AbfPeekChannel <- function(abf, intv = NULL, channel = 1L, sample_ratio = 100L,
                           cursor = NULL, time_unit = "tick", colour = TRUE, auto_zoom = FALSE,
                           title = NULL, ...) {

  AbfPlotChannel(abf, intv = intv, channel = channel,
                 cursor = cursor, time_unit = time_unit, colour = colour, auto_zoom = auto_zoom,
                 title = title, sample_ratio = sample_ratio, ...)
}

#' Plot channel vs channel data of abf objects.
#'
#' @param abf an abf object/a list of abf objects
#' @param intv an intv to evaluate channel values.
#' @param x_channel channel of x axis.
#' @param y_channel channel of y axis.
#' @param map_func a function to evaluate channel values.
#' @param colour whether to plot in colour mode.
#' @param title add title to the plot.
#' @param legend_title add legend title to the plot.
#' @param zero_axes whether to add zeroed XY axes to the plot.
#' @param ... passed to map_func()
#'
#' @return a ggplot object.
#' @export
#'
AbfPlotXY <- function(abf, intv = NULL, x_channel = 2L, y_channel = 1L, map_func = "mean",
                      colour = TRUE, title = NULL, legend_title = "", zero_axes = FALSE,
                      ...) {

  channel <- c(x_channel, y_channel)
  CheckArgs(abf, chan = channel, allow_list = TRUE)

  p <- abf_plot_cd(abf, intv = intv, channel = channel, map_func = map_func,
                   colour = colour, ...)

  if (IsAbfList(abf)) {
    label <- DefaultChanLabel(abf[[1]])
  } else {
    label <- DefaultChanLabel(abf)
  }
  xlabel <- label[x_channel]
  ylabel <- label[y_channel]

  if (!is.null(title)) {
    p <- p + labs(x = xlabel, y = ylabel, title = title)
  } else {
    p <- p + labs(x = xlabel, y = ylabel)
  }
  if (colour) {
    p <- p + labs(colour = legend_title)
  } else {
    p <- p + labs(group = legend_title)
  }

  if (zero_axes) {
    ApplyZeroAxes(p + geom_line())
  } else {
    p + geom_line()
  }
}

#' @rdname AbfPlotXY
#' @export
#'
AbfPlotIV <- function(abf, intv = NULL, map_func = "mean",
                      colour = TRUE, title = NULL, legend_title = "", zero_axes = TRUE,
                      ...) {

  CheckArgs(abf, allow_list = TRUE)
  x_channel <- GetFirstVoltageChan(abf)
  y_channel <- GetFirstCurrentChan(abf)

  AbfPlotXY(abf, intv = intv, map_func = map_func,
            x_channel = x_channel, y_channel = y_channel,
            colour = colour, title = title, legend_title = legend_title, zero_axes = zero_axes,
            ...)
}
