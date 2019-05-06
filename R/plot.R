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
PlotChannelAbf <- function(abf, intv = NULL, channel = GetAllChannels(abf),
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

#' @rdname PlotChannelAbf
#' @export
#'
PeekChannelAbf <- function(abf, intv = NULL, channel = 1L, sample_ratio = 100L,
                           cursor = NULL, time_unit = "tick", colour = TRUE, auto_zoom = FALSE,
                           title = NULL, ...) {

  PlotChannelAbf(abf, intv = intv, channel = channel,
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
PlotXYAbf <- function(abf, intv = NULL, x_channel = 2L, y_channel = 1L, map_func = "mean",
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

#' @rdname PlotXYAbf
#' @export
#'
PlotIVAbf <- function(abf, intv = NULL, map_func = "mean",
                      colour = TRUE, title = NULL, legend_title = "", zero_axes = TRUE,
                      ...) {

  CheckArgs(abf, allow_list = TRUE)
  x_channel <- GetFirstVoltageChan(abf)
  y_channel <- GetFirstCurrentChan(abf)

  PlotXYAbf(abf, intv = intv, map_func = map_func,
            x_channel = x_channel, y_channel = y_channel,
            colour = colour, title = title, legend_title = legend_title, zero_axes = zero_axes,
            ...)
}

#' Plot IV Summary generated by IVSummary().
#'
#' @details plotting mode can be i, g, spi, spg, which stand for current, conductance,
#' specific current and specific conducatance and the plotting function
#' looks for Current/Conductance/SpCurrent/SpConductance column respectively.
#'
#' PlotIVSummary() uses basic splines to achieve smoothing. However, b-spline
#' smoothing can be misleading especially when I-V follows logistic and hyperbolic
#' relationship. In these cases, you should always use a suitable fitting formula
#' to smooth.
#'
#' @param ivs a data.frame generated by IVSummary or a list of such data.frames.
#' @param mode plotting mode, see details for more information.
#' @param colour whether to plot in colour mode.
#' @param title title of the plot.
#' @param legend_title title of the legend (group).
#' @param error_bar whether to plot error bars.
#' @param zero_axes wheter to apply ZeroAxes() to plot.
#' @param smooth wheter to plot b-Spline smoothed lines, see details for more information.
#' @param ... not used.
#'
#' @return a ggplot object.
#' @export
#'
PlotIVSummary <- function(ivs, mode = c("i", "g", "spi", "spg"),
                          colour = TRUE, title = NULL, legend_title = "",
                          error_bar = FALSE, zero_axes = TRUE, smooth = FALSE, ...) {

  mode <- match.arg(mode)

  if (is.list(ivs) && !is.data.frame(ivs)) {
    if (IsListOf(ivs, "data.frame")) {
      if (!"Group" %in% names(ivs[[1]])) {
        warning("No groups defined in ivs, adding default groups.")
        for (idx in seq_along(ivs)) {
          ivs[[idx]]$Group <- paste0("grp", idx)
        }
      }
      ivs <- do.call(rbind, ivs)
    } else {
      stop("ivs should be a data.frame or a list of data.frame")
    }
  }

  x <- list(
    col = "Voltage",
    sem = "SEM_Voltage",
    unit = ivs$Unit_Voltage[1],
    symb = "V"
  )
  y <- switch(mode,
              i = list(
                col = "Current",
                sem = "SEM_Current",
                unit = ivs$Unit_Current[1],
                symb = "I"
              ),
              g = list(
                col = "Conductance",
                sem = "SEM_Conductance",
                unit = ivs$Unit_Conductance[1],
                symb = "G"
              ),
              spi = list(
                col = "SpCurrent",
                sem = "SEM_SpCurrent",
                unit = ivs$Unit_SpCurrent[1],
                symb = "Specific I"
              ),
              spg = list(
                col = "SpConductance",
                sem = "SEM_SpConductance",
                unit = ivs$Unit_SpConductance[1],
                symb = "Specific G"
              ))

  aes_args <- list(x = x$col, y = y$col)

  if ("Group" %in% names(ivs)) {
    if (colour) {
      aes_args$colour = "Group"
    } else {
      aes_args$group = "Group"
    }
    if (error_bar) {
      aes_args$shape = "Group"
    }
    min_pts <- min(stats::aggregate(Voltage ~ Group, data = ivs, FUN = length)$Voltage)
  } else {
    min_pts <- nrow(ivs)
  }

  p <- ggplot(ivs, mapping = do.call(aes_string, aes_args))

  if (error_bar) {
    delta <- (max(ivs$Voltage) - min(ivs$Voltage)) / min_pts
    p <- p +
      geom_point() +
      geom_errorbar(aes_string(ymin = sprintf("%s - %s", y$col, y$sem),
                               ymax = sprintf("%s + %s", y$col, y$sem)),
                    width = delta / 8)
  }

  if (smooth) {
    if (colour) {
      p <- p + geom_smooth(se = FALSE, method = "lm",
                           formula = y ~ splines::bs(x))
    } else {
      p <- p + geom_smooth(colour = "Black", se = FALSE, method = "lm",
                           formula = y ~ splines::bs(x))
    }
  } else {
    p <- p + geom_line()
  }

  if (is.null(x$unit)) {
    xlabel <- x$col
  } else {
    xlabel <- GetAxisLabel(x$symb, x$unit, style = "%s (%s)")
  }
  if (is.null(y$unit)) {
    ylabel <- y$col
  } else {
    ylabel <- GetAxisLabel(y$symb, y$unit, style = "%s (%s)")
  }
  if (!is.null(title)) {
    p <- p + labs(x = xlabel, y = ylabel, title = title)
  } else {
    p <- p + labs(x = xlabel, y = ylabel)
  }
  p <- p + labs(colour = legend_title,
                shape = legend_title,
                group = legend_title)

  if (zero_axes) {
    ApplyZeroAxes(p)
  } else {
    p
  }
}
