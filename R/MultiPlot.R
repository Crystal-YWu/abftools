#' Title
#'
#' @param abf_list
#' @param channel
#' @param colour
#' @param time_unit
#' @param num_label
#' @param title_label
#' @param unify_y
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
MultiPlotChannel <- function(abf_list, channel = 1, colour = FALSE, time_unit = "tick",
                             num_label = FALSE, title_label = TRUE, unify_y = TRUE, ...) {

  p <- MultiPlotP(abf_list, channel, colour, time_unit, num_label, title_label,
                  unify_y, ...)

  n <- ceiling(sqrt(length(abf_list)))
  return(plot_grid(plotlist = p, ncol = n))
}

#' Title
#'
#' @param abf_list
#' @param channel
#' @param colour
#' @param time_unit
#' @param num_label
#' @param title_label
#' @param unify_y
#' @param ratio
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
MultiPeekChannel <- function(abf_list, channel = 1, colour = FALSE, time_unit = "tick",
                             num_label = FALSE, title_label = TRUE, unify_y = TRUE,
                             ratio = 100, ...) {

  p <- MultiPlotChannel(abf_list, channel, colour, time_unit, num_label, title_label,
                        unify_y, sampling_ratio = ratio, ...)
  return(p)
}

#' Title
#'
#' @param abf_list
#' @param intv_list
#' @param channel
#' @param colour
#' @param time_unit
#' @param num_label
#' @param title_label
#' @param unify_y
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param abf_list
#' @param intv_list
#' @param channel
#' @param colour
#' @param time_unit
#' @param num_label
#' @param title_label
#' @param unify_y
#' @param ratio
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
MultiPeekChannel_Intv <- function(abf_list, intv_list, channel = 1, colour = FALSE,
                                  time_unit = "tick", num_label = FALSE, title_label = TRUE,
                                  unify_y = TRUE, ratio = 100, ...) {

  p <- MultiPlotChannel_Intv(abf_list, intv_list, channel, colour, time_unit,
                             num_label, title_label, unify_y, sampling_ratio = ratio, ...)
  return(p)
}

#' Title
#'
#' @param abf_list
#' @param cursor_list
#' @param channel
#' @param colour
#' @param time_unit
#' @param num_label
#' @param title_label
#' @param unify_y
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param abf_list
#' @param cursor_list
#' @param channel
#' @param colour
#' @param time_unit
#' @param num_label
#' @param title_label
#' @param unify_y
#' @param ratio
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
MultiPeekChannel_Cursor <- function(abf_list, cursor_list, channel = 1, colour = FALSE,
                                  time_unit = "tick", num_label = FALSE, title_label = TRUE,
                                  unify_y = TRUE, ratio = 100, ...) {

  p <- MultiPlotChannel_Cursor(abf_list, cursor_list, channel, colour, time_unit,
                             num_label, title_label, unify_y, sampling_ratio = ratio, ...)
  return(p)
}

#' Title
#'
#' @param abf_list
#' @param channel
#' @param time_unit
#' @param title_label
#' @param spacing
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
