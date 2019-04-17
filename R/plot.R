#' Plot channel data of abf objects.
#'
#' @description AbfPlotChannel() is a unified wrapper to abf_plot_td(), it is
#' capable of plotting multiple abf object facetted by id, multiple channels
#' facetted by channel.
#'
#' @param abf an abf object/a list of abf objects
#' @param intv an INDEX intv to sample abf.
#' @param channel channels to plot.
#' @param sample_ratio used in AbfPeekChannel, to reduce data points by sample_ratio.
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
AbfPlotChannel <- function(abf, intv = NULL, channel = 1L,
                           cursor = NULL, time_unit = "tick", colour = FALSE, auto_zoom = FALSE,
                           title = NULL, ...) {

  CheckArgs(abf, chan = channel, allow_list = TRUE)

  p <- abf_plot_td(abf = abf, intv = intv, channel = channel,
                   time_unit = time_unit, colour = colour, ...)


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
      ylabel <- GetChanLabel(abf[[1]], style = "%s (%s)")
    }
  } else {
    if (length(channel) > 1L) {
      p <- p + facet_grid(stats::reformulate(".", "Channel"),
                          labeller = DefaultLabeller(abf),
                          scales = "free_y")
      ylabel <- "Channel"
    } else {
      ylabel <- GetChanLabel(abf, style = "%s (%s)")
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
                           title = NULL) {

  AbfPlotChannel(abf, intv = intv, channel = channel,
                 cursor = cursor, time_unit = time_unit, colour = colour, auto_zoom = auto_zoom,
                 title = title, sample_ratio = sample_ratio)
}
