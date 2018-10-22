#' Plot a channel of multiple abf objects.
#'
#' @param abf_list a list of abf objects.
#' @param channel channel id, 1-based.
#' @param colour whether to plot in coloured mode.
#' @param time_unit time unit to use in plot.
#' @param num_label whether to show a numbered label of each abf object.
#' @param title_label wheter to show title of each abf object.
#' @param unify_y wheter to unifier y scale of each abf object.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
MultiPlotChannel <- function(abf_list, channel = 1, colour = FALSE, time_unit = "tick",
                             num_label = FALSE, title_label = TRUE, unify_y = TRUE, ...) {

  p <- MultiPlotP(abf_list, channel, colour, time_unit, num_label, title_label,
                  unify_y, ...)

  n <- ceiling(sqrt(length(abf_list)))
  return(plot_grid(plotlist = p, ncol = n))
}

#' Fast plot a channel of multiple abf objects by sampling points.
#'
#' @param abf_list a list of abf objects.
#' @param channel channel id, 1-based.
#' @param colour whether to plot in coloured mode.
#' @param time_unit time unit to use in plot.
#' @param num_label whether to show a numbered label of each abf object.
#' @param title_label wheter to show title of each abf object.
#' @param unify_y wheter to unifier y scale of each abf object.
#' @param ratio sampling ratio for faster plotting.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
MultiPeekChannel <- function(abf_list, channel = 1, colour = FALSE, time_unit = "tick",
                             num_label = FALSE, title_label = TRUE, unify_y = TRUE,
                             ratio = 100, ...) {

  p <- MultiPlotChannel(abf_list, channel, colour, time_unit, num_label, title_label,
                        unify_y, sampling_ratio = ratio, ...)
  return(p)
}

#' Plot a channel of multiple abf objects, with intervals.
#'
#' @param abf_list a list of abf objects.
#' @param intv_list a list of intervals.
#' @param channel channel id, 1-based.
#' @param colour whether to plot in coloured mode.
#' @param time_unit time unit to use in plot.
#' @param num_label whether to show a numbered label of each abf object.
#' @param title_label wheter to show title of each abf object.
#' @param unify_y wheter to unifier y scale of each abf object.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
MultiPlotChannel_Intv <- function(abf_list, intv_list, channel = 1, colour = FALSE,
                                  time_unit = "tick", num_label = FALSE, title_label = TRUE,
                                  unify_y = TRUE, ...) {

  p <- MultiPlotP(abf_list, channel, colour, time_unit, num_label, title_label,
                  unify_y, ...)
  for (i in seq_along(p)) {
    #convert intv to time_unit
    intv <- TickToTime(abf_list[[i]], time_unit, intv_list[[i]])
    p[[i]] <- p[[i]] +
      geom_vline(xintercept = intv[1:2], linetype = "dashed")
  }

  n <- ceiling(sqrt(length(abf_list)))
  return(plot_grid(plotlist = p, ncol = n))
}

#' Fast plot a channel of multiple abf objects, with intervals.
#'
#' @param abf_list a list of abf objects.
#' @param intv_list a list of intervals.
#' @param channel channel id, 1-based.
#' @param colour whether to plot in coloured mode.
#' @param time_unit time unit to use in plot.
#' @param num_label whether to show a numbered label of each abf object.
#' @param title_label wheter to show title of each abf object.
#' @param unify_y wheter to unifier y scale of each abf object.
#' @param ratio sampling ratio for faster plotting.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
MultiPeekChannel_Intv <- function(abf_list, intv_list, channel = 1, colour = FALSE,
                                  time_unit = "tick", num_label = FALSE, title_label = TRUE,
                                  unify_y = TRUE, ratio = 100, ...) {

  p <- MultiPlotChannel_Intv(abf_list, intv_list, channel, colour, time_unit,
                             num_label, title_label, unify_y, sampling_ratio = ratio, ...)
  return(p)
}

#' Plot a channel of multiple abf objects, with curosrs.
#'
#' @param abf_list a list of abf objects.
#' @param cursor_list a list of cursors.
#' @param channel channel id, 1-based.
#' @param colour whether to plot in coloured mode.
#' @param time_unit time unit to use in plot.
#' @param num_label whether to show a numbered label of each abf object.
#' @param title_label wheter to show title of each abf object.
#' @param unify_y wheter to unifier y scale of each abf object.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
MultiPlotChannel_Cursor <- function(abf_list, cursor_list, channel = 1, colour = FALSE,
                                  time_unit = "tick", num_label = FALSE, title_label = TRUE,
                                  unify_y = TRUE, ...) {

  p <- MultiPlotP(abf_list, channel, colour, time_unit, num_label, title_label,
                  unify_y, ...)
  for (i in seq_along(p)) {
    #convert intv to time_unit
    cursor <- TickToTime(abf_list[[i]], time_unit, cursor_list[[i]])
    p[[i]] <- p[[i]] +
      geom_vline(xintercept = cursor, linetype = "dashed")
  }

  n <- ceiling(sqrt(length(abf_list)))
  return(plot_grid(plotlist = p, ncol = n))
}

#' Fast plot a channel of multiple abf objects, with curosrs.
#'
#' @param abf_list a list of abf objects.
#' @param cursor_list a list of cursors.
#' @param channel channel id, 1-based.
#' @param colour whether to plot in coloured mode.
#' @param time_unit time unit to use in plot.
#' @param num_label whether to show a numbered label of each abf object.
#' @param title_label wheter to show title of each abf object.
#' @param unify_y wheter to unifier y scale of each abf object.
#' @param ratio sampling ratio for faster plotting.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
MultiPeekChannel_Cursor <- function(abf_list, cursor_list, channel = 1, colour = FALSE,
                                  time_unit = "tick", num_label = FALSE, title_label = TRUE,
                                  unify_y = TRUE, ratio = 100, ...) {

  p <- MultiPlotChannel_Cursor(abf_list, cursor_list, channel, colour, time_unit,
                             num_label, title_label, unify_y, sampling_ratio = ratio, ...)
  return(p)
}

#' Plot a channel of multiple abf objects, in a common publication standard.
#'
#' @param abf_list a list of abf objects.
#' @param channel channel id, 1-based.
#' @param time_unit time unit to use in plot.
#' @param title_label whether to show title for each abf object.
#' @param spacing tweak space between two plot.
#' @param ... other arguments, see melt.abf for more details.
#'
#' @return a ggplot object.
#' @export
#'
MultiPlotChannel_Pub <- function(abf_list, channel = 1, time_unit = "s", title_label = FALSE,
                                 spacing = 0, ...) {

  p <- MultiPlotP(abf_list, channel, FALSE, time_unit, FALSE, title_label, TRUE, ...)
  for (i in seq_along(p))
    p[[i]] <- p[[i]] + theme_classic()

  #first plot, with axes and units
  f <- function(x) sprintf("%.2f", x)
  maxt <- TickToTime(abf_list[[1]], time_unit, nPts(abf_list[[1]]))
  p[[1]] <- p[[1]] +
    scale_x_continuous(breaks = c(0, maxt), labels = f)
  #TODO: following plots are a bit mis-alined due to the removal of axes
  #Current workaround: set x colour to white, and using y title to work as a
  #spacing tool.
  for (i in 2:length(p)) {
    p[[i]] <- p[[i]] + theme(axis.title.x = element_text(colour = "white"),
                             axis.text.x = element_text(colour = "white"),
                             axis.line.x = element_line(colour = "white"),
                             axis.ticks.x = element_line(colour = "white"),
                             axis.title.y = element_text(colour = "white", size = spacing),
                             axis.text.y = element_blank(),
                             axis.line.y = element_blank(),
                             axis.ticks.y = element_blank())
  }

  return(plot_grid(plotlist = p, nrow = 1))
}
