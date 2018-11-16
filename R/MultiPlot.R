#' Plot a channel of multiple abf objects, in a common publication standard.
#'
#' @param abf_list a list of abf objects.
#' @param channel channel id, 1-based.
#' @param time_unit time unit to use in plot.
#' @param manual_title titles for subplots.
#' @param auto_title automatically add titles for subplots.
#' @param x_ticks_pos a vector of positions (valued between 0 and 1) to show x ticks.
#' @param ... other arguments passed, see PlotChannel for more details.
#'
#' @return a ggplot object.
#' @export
#'
MultiPlotChannel_Pub <- function(abf_list, channel = 1L, time_unit = "s",
                                 manual_title = NULL, auto_title = FALSE,
                                 x_ticks_pos = c(0.2, 0.8), ...) {

  if (!IsAbfList(abf_list)) {
    err_class_abf_list()
  }
  channel <- FirstElement(channel)
  for (tmp in abf_list) {
    if (!AssertChannel(tmp, channel)) {
      err_channel()
    }
  }

  manual_title <- unlist(manual_title)
  if (!is.null(manual_title)) {
    if (!AssertLength(manual_title, abf_list)) {
      err_assert_len(manual_title, abf_list)
    }
  } else if (auto_title) {
    manual_title <- unlist(GetTitle(abf_list))
  }

  p <- list()
  for (i in seq_along(abf_list)) {
    p[[i]] <- PlotChannel(abf_list[[i]], channel = channel, time_unit = time_unit,
                          title = manual_title[i], ...)
  }

  #first plot, with axes and units
  f <- function(x) sprintf("%.2f", x)
  maxt <- TickToTime(abf_list[[1]], time_unit, nPts(abf_list[[1]]))
  p[[1]] <- p[[1]] +
    scale_x_continuous(breaks = maxt * x_ticks_pos, labels = f)

  for (i in 2:length(p)) {
    p[[i]] <- p[[i]] + theme(axis.title.x = element_blank(),
                             axis.text.x = element_blank(),
                             axis.line.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             axis.title.y = element_blank(),
                             axis.text.y = element_blank(),
                             axis.line.y = element_blank(),
                             axis.ticks.y = element_blank())
  }

  return(plot_grid(plotlist = p, nrow = 1, align = "h"))
}

#' Plot channel/channels of multiple abf objects.
#'
#' @param abf_list a list of abf objects.
#' @param channel channel/channels to plot, 1-based.
#' @param intv OPTIONAL, an interval (a vector of c(start, end, len))/a list of intervals to plot on top.
#' @param cursor  OPTIONAL, cursors (a vector of positions)/a list of cursors to plot on top.
#' @param colour whether to plot in coloured mode.
#' @param time_unit time unit for x axis.
#' @param auto_zoom whether to zoom in the plot automatically.
#' @param manual_title OPTIONAL, titles of every abf plot.
#' @param auto_title OPTIONAL, whether to automatically add titles for every abf plot.
#' @param num_label OPTIONAL, whether to add number label to every abf plot.
#' @param channel_arrange arrangement of channel subplots.
#' @param plot_arrange arrangement of abf plots.
#' @param ...
#'
#' @return a ggplot object.
#' @export
#'
MultiPlotChannel <- function(abf_list, channel = 1L, intv = NULL, cursor = NULL,
                             colour = FALSE, time_unit = "tick", auto_zoom = FALSE,
                             manual_title = NULL, auto_title = FALSE, num_label = FALSE,
                             channel_arrange = "H", plot_arrange = "A", ...) {

  if (!IsAbfList(abf_list)) {
    err_class_abf_list()
  }
  #check selected channels are valid for all abf objects in abf_list
  channel <- unlist(channel)
  for (abf in abf_list) {
    if (!AssertChannel(abf, channel)) {
      err_channel()
    }
  }
  #check intv and cursor
  if (is.list(intv)) {
    if (!AssertLength(intv, abf_list)) {
      err_assert_len(intv, abf_list)
    }
  } else {
    #convert intv to a list of intv with same length of abf_list
    intv <- rep(list(intv), length(abf_list))
  }
  if (is.list(cursor)) {
    if (!AssertLength(cursor, abf_list)) {
      err_assert_len(cursor, abf_list)
    }
  } else {
    #convert cursor to a list of cursor with same length of abf_list
    cursor <- rep(list(cursor), length(abf_list))
  }

  #generate proper subtitles
  has_subtitles <- FALSE
  #unlist in case of a list of NULL
  manual_title <- unlist(manual_title)
  if (!is.null(manual_title)) {
    #subtitles are given, check length of subtitles
    if (!AssertLength(manual_title, abf_list)) {
      err_assert_len(manual_title, abf_list)
    }
    has_subtitles <- TRUE
  } else if (auto_title) {
    #subtitles are not given, however auto_title enabled
    manual_title <- unlist(GetTitle(abf_list))
    has_subtitles <- TRUE
  } else {
    #subtitles are not given nor auto_title enabled
    manual_title <- rep("", length(abf_list))
  }
  if (num_label) {
    #attach number labels
    manual_title <- paste0(seq_along(abf_list), ". ", manual_title)
    has_subtitles <- TRUE
  }

  multi_channel <- length(channel) > 1L
  plist <- list()
  for (i in seq_along(abf_list)) {

    #collect channel plots
    subp <- PlotChannel(abf_list[[i]], intv = intv[[i]], cursor = cursor[[i]],
                        channel = channel, colour = colour, time_unit = time_unit,
                        auto_zoom = auto_zoom, title = NULL, ...)
    if (multi_channel) {
      if (has_subtitles) {
        subp[[1]] <- subp[[1]] + ggtitle(manual_title[i])
      }
      subp <- ArrangePlot(subp, channel_arrange)
    } else if (has_subtitles) {
      subp <- subp + ggtitle(manual_title[i])
    }

    plist[[i]] <- subp
  }

  p <- ArrangePlot(plist, plot_arrange)
  return(p)
}

#' Fast plot channel/channels of multiple abf objects.
#'
#' @param abf_list a list of abf objects.
#' @param channel channel/channels to plot, 1-based.
#' @param intv OPTIONAL, an interval (a vector of c(start, end, len))/a list of intervals to plot on top.
#' @param cursor  OPTIONAL, cursors (a vector of positions)/a list of cursors to plot on top.
#' @param colour whether to plot in coloured mode.
#' @param time_unit time unit for x axis.
#' @param auto_zoom whether to zoom in the plot automatically.
#' @param subtitles OPTIONAL, titles of every abf plot.
#' @param auto_title OPTIONAL, whether to automatically add titles for every abf plot.
#' @param num_label OPTIONAL, whether to add number label to every abf plot.
#' @param channel_arrange arrangement of channel subplots.
#' @param plot_arrange arrangement of abf plots.
#' @param ...
#'
#' @return a ggplot object.
#' @export
#'
MultiPeekChannel <- function(abf_list, channel = 1L, intv = NULL, cursor = NULL,
                             colour = FALSE, time_unit = "tick", auto_zoom = FALSE,
                             manual_title = NULL, auto_title = FALSE, num_label = FALSE,
                             channel_arrange = "H", plot_arrange = "A", ratio = 100L,
                             ...) {

  p <- MultiPlotChannel(abf_list, channel = channel, intv = intv, cursor = cursor,
                        colour = colour, time_unit = time_unit, auto_zoom = auto_zoom,
                        manual_title = manual_title, auto_title = auto_title,
                        num_label = num_label, channel_arrange = channel_arrange,
                        plot_arrange = plot_arrange, sampling_ratio = ratio, ...)

  return(p)
}
