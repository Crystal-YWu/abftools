#' Quick plot various type of data.
#'
#' QuickPlot automatically select proper plotting functions according to the type
#' of data and arguments provided. You can always try QuickPlot first if you don't
#' know how to plot or what plotting function to use before consulting manual.
#'
#' When plotting an abf object, the type of generated plot is determined by this
#' logic: 1. Whether the abf object has only one channel, if so, a channel plot is
#' generated. 2. Whether time_unit is given, if so, plot all channels and convert
#' x axis to time_unit. 3. Whether the abf object has only one episode, if so, a
#' channel plot of time unit "tick" is generated.
#'
#' @param data data to plot, QuickPlot will try its best to figure out how to plot.
#' @param pos positions to sample and calculate values, can be interval or cursor.
#' @param intv interval compatible with PlotChannel(), also used to sample and
#' calculate values.
#' @param cursor cursor compatible with PlotChannel(), also used to sample and
#' calculate values.
#' @param time_unit unit of time.
#' @param colour whether to plot in colour mode.
#' @param title OPTIONAL, title for the plot.
#' @param legend_title OPTIONAL, legend title for the plot.
#' @param zero_intercept whether to add zero intercepts.
#' @param zero_axes whether to force position axes at 0.
#' @param line_size size of lines.
#' @param marker_size size of markers, not used when plotting abf objects.
#' @param err_bar_size size of error bars, not used when plotting abf objects.
#' @param err_bar_width width of error bars, not used when plotting abf objects.
#' @param ... not used.
#'
#' @return a ggplot object
#' @export
#'
QuickPlot <- function(data, ...) {

  UseMethod("QuickPlot", data)
}

#' @rdname QuickPlot
#' @export
#'
QuickPlot.default <- function(data, ...) {

  err_quick_plot()
}

#' @rdname QuickPlot
#' @export
#'
#' @method QuickPlot abf
#'
QuickPlot.abf <- function(abf, pos, intv = NULL, cursor = NULL, time_unit,
                          colour = FALSE, title = NULL, legend_title = NULL,
                          zero_intercept = TRUE, zero_axes = TRUE,
                          line_size = 0.5) {


  if (missing(time_unit)) {

    if (nChan(abf) == 1L || nEpi(abf) == 1L) {
      #time_unit is not defined, but only one channel/episode,
      #only time series is possible
      p <- PeekAllChannel(abf, intv = intv, cursor = cursor, colour = colour,
                          auto_zoom = TRUE)
    } else {
      abf <- list(abf)

      #parse pos
      #TODO: potential performance implication
      if (missing(pos)) {
        pos <- NULL
      }
      if (!is.null(pos)) {
        pos <- MaskIntv(pos)
      }
      if (!is.null(intv)) {
        pos <- c(pos, MaskIntv(intv))
      }
      if (!is.null(cursor)) {
        pos <- c(pos, cursor)
      }
      pos <- unique(pos)

      p <- QuickPlot.list(abf, pos = pos, colour = colour,
                     title = title, legend_title = legend_title,
                     zero_intercept = zero_intercept, zero_axes = zero_axes,
                     line_size = line_size)
    }

  } else {

    if (!missing(pos) && !is.null(pos)) {
      warning("Argument pos is ignored. Please use intv or cursor for time series plots.")
    }

    #time_unit is explicitly defined, plot time series.
    p <- PeekAllChannel(abf, intv = intv, cursor = cursor, colour = colour,
                        time_unit = time_unit, auto_zoom = TRUE)
  }

  p
}

#' @rdname QuickPlot
#' @export
#'
#' @method QuickPlot data.frame
#'
QuickPlot.data.frame <- function(df, colour = FALSE,
                                 title = NULL, legend_title = NULL,
                                 zero_intercept = TRUE, zero_axes = TRUE,
                                 line_size = 0.5, marker_size = line_size * 4,
                                 err_bar_size = line_size / 1.5,
                                 err_bar_width = marker_size * 1.5) {

  df <- list(df)

  QuickPlot.list(df, colour = colour,
                 title = title, legend_title = legend_title,
                 zero_intercept = zero_intercept, zero_axes = zero_axes,
                 line_size = line_size, marker_size = marker_size,
                 err_bar_size = err_bar_size, err_bar_width = err_bar_width)
}

#' @rdname QuickPlot
#' @export
#'
#' @method QuickPlot list
#'
QuickPlot.list <- function(data, pos = NULL, colour = TRUE,
                           title = NULL, legend_title = NULL,
                           zero_intercept = TRUE, zero_axes = TRUE,
                           line_size = 0.5, marker_size = line_size * 4,
                           err_bar_size = line_size / 1.5,
                           err_bar_width = marker_size * 1.5) {

  #whether column id is present in data
  id_present <- FALSE

  #Data
  if (IsListOf(data, "abf")) {

    pos <- ExpandList(pos, data)
    if (is.null(pos)) {
      err_assert_len(pos, data)
    }

    plt_data <- list()
    for (i in seq_along(data)) {
      abf <- data[[i]]
      intv <- pos[[i]]
      plt_data[[GetTitle(abf)]] <- ParseDataFrameIV(mean(abf, intv = intv))
    }

    #Extract unit information from first abf object
    chan_label <- DefaultChanLabel(data[[1]])
    chan_v <- GetFirstVoltageChan(data[[1]])
    chan_c <- GetFirstCurrentChan(data[[1]])

    x_label <- chan_label[chan_v]
    y_label <- chan_label[chan_c]

  } else if (IsListOf(data, "data.frame") || IsListOf(data, "matrix")) {

    id_present <-  all(sapply(data,
                              function(x) match("id", colnames(x), nomatch = 0L) > 0L))
    plt_data <- lapply(data, ParseDataFrameIV, has_id = id_present)
    for (i in seq_along(plt_data)) {
      if (all(is.na(plt_data[[i]]$Voltage))) {
        err_quick_plot("No voltage data found.")
      }
      if (all(is.na(plt_data[[i]]$Current))) {
        err_quick_plot("No voltage data found.")
      }
    }
    if (!id_present) {
      plt_data <- EnforceListNames(plt_data)
    }

    #We do not have unit information in this case
    x_label <- "Voltage"
    y_label <- "Current"

  } else {

    err_quick_plot("Element type not supported.")
  }

  #row bind data
  if (id_present) {
    df <- do.call(rbind, plt_data)
  } else {
    df <- BindDataFrameList(plt_data)
  }
  err_bar <- any(!is.na(df$SEMC))

  #Mapping
  if (colour) {
    if (err_bar) {
      p <- ggplot(df, aes(x = Voltage, y = Current, colour = id, shape = id))
    } else {
      p <- ggplot(df, aes(x = Voltage, y = Current, colour = id))
    }
  } else {
    if (err_bar) {
      p <- ggplot(df, aes(x = Voltage, y = Current, group = id, shape = id))
    } else {
      p <- ggplot(df, aes(x = Voltage, y = Current, group = id))
    }
  }

  #Plotting

  #plot lines
  p <- p + geom_line(size = line_size)

  #plot error bars
  if (err_bar) {
    #SEM Current is available
    p <- p +
      geom_point(size = marker_size) +
      geom_errorbar(aes(ymin = Current - SEMC, ymax = Current + SEMC),
                    size = err_bar_size, width = err_bar_width)
  }

  #plot axes
  p <- p + theme_classic()
  if (zero_axes) {
    p <- p + ZeroAxes(xlimit = df$Voltage, ylimit = df$Current,
                      xlabel = x_label, ylabel = y_label)
  } else {
    p <- p +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      xlab(x_label) +
      ylab(y_label)
  }

  #plot titles
  if (!is.null(title)) {
    p <- p + ggtitle(as.character(title))
  }
  if (is.null(legend_title)) {
    p <- p + theme(legend.title = element_blank())
  } else {
    p <- p + labs(colour = as.character(legend_title),
                  shape = as.character(legend_title),
                  group = as.character(legend_title))
  }

  #remove legend if only one item
  if (!id_present && length(plt_data) == 1L) {
    p <- p + theme(legend.position = "none")
  }

  p
}

#' Quick plot I-V curves at given position
#'
#' @param abf an abf or a list of abf objects.
#' @param pos an interval/cursor or a list of intervals/cursors.
#' @param colour whether to plot in coloured mode.
#' @param title OPTIONAL, title for the plot.
#' @param legend_title OPTIONAL, title of the legend.
#' @param zero_intercept whether to add zero intercepts to the plot.
#' @param zero_axes whether to plot axes at 0.
#' @param line_size size of lines
#'
#' @return a ggplot object.
#' @export
#'
QuickPlotIV <- function(abf, pos, colour = FALSE, title = NULL, legend_title = NULL,
                        zero_intercept = TRUE, zero_axes = TRUE, line_size = 0.5) {

  if (!IsAbf(abf) && !IsAbfList(abf)) {
    err_class_abf()
  }

  if (IsAbf(abf)) {
    current_channel <- GetFirstCurrentChan(abf)
    voltage_channel <- GetFirstVoltageChan(abf)
    labs <- DefaultChanLabel(abf)
  } else {
    current_channel <- GetFirstCurrentChan(abf[[1]])
    voltage_channel <- GetFirstVoltageChan(abf[[1]])
    labs <- DefaultChanLabel(abf[[1]])
  }

  melted <- MeltAbfChannel(abf, channel = c(voltage_channel, current_channel),
                           intv = pos, epi_id_func = NULL)

  cname <- colnames(melted)
  xcol <- as.name(cname[2L])
  ycol <- as.name(cname[3L])
  p <- ggplot(melted, aes_string(x = xcol, y = ycol)) + theme_classic()
  if (colour) {
    p <- p + geom_line(aes_string(colour = "id"), size = line_size)
    if (is.null(legend_title)) {
      p <- p + theme(legend.title = element_blank())
    } else {
      p <- p + labs(`colour` = as.character(legend_title))
    }
  } else {
    p <- p + geom_line(aes_string(group = "id"), size = line_size)
  }

  #Add zero intercepts
  if (!zero_axes && zero_intercept) {
    p <- p + geom_vline(xintercept = 0, linetype = "dashed") +
      geom_hline(yintercept = 0, linetype = "dashed")
  }
  if (zero_axes) {
    p <- p + ZeroAxes(xlimit = melted[, 2], ylimit = melted[, 3],
                      xlabel = labs[voltage_channel], ylabel = labs[current_channel])
  }

  #Get rid of ``
  p <- p + xlab(as.character(xcol)) + ylab(as.character(ycol))

  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }

  return(p)

}

#' Quick plot trace vs. trace curve.
#'
#' @param abf an abf object.
#' @param channelX channel of X trace.
#' @param episodeX episode of X trace.
#' @param channelY channel of Y trace.
#' @param episodeY channel of Y trace.
#' @param intv interval of the traces to be plotted.
#'
#' @return a ggplot object.
#' @export
#'
QuickPlotTrace <- function(abf, channelX, episodeX, channelY, episodeY, intv) {

  if (!IsAbf(abf)) {
    err_class_abf()
  }
  if (!AssertChannel(abf, channelX) || !AssertChannel(abf, channelY)) {
    err_channel()
  }
  if (!AssertEpisode(abf, episodeX) || !AssertEpisode(abf, episodeY)) {
    err_epi()
  }

  chX <- abf[[channelX]]
  chY <- abf[[channelY]]
  mask <- intv[1]:intv[2]
  traceX <- chX[mask, episodeX]
  traceY <- chY[mask, episodeY]

  qplot(x = traceX, y = traceY, geom = "point") + theme_bw()
}

#' Quick plot an IV Summary table
#'
#' @param df_summary an IV summary table from IVSummary()
#' @param title OPTIONAL, title of the plot
#' @param legend_title OPTIONAL, if multiple IV summaries are to plot, title of the legend.
#' @param zero_intercept whether to add zero intercepts to the plot.
#' @param zero_axes whether to plot axes at 0.
#' @param line_size size of lines
#' @param marker_size size of markers
#' @param err_bar_size size of error bar
#' @param err_bar_width width of error bar
#'
#' @return a ggplot object.
#' @export
#'
QuickPlot_IVSummary <- function(df_summary, title = NULL, legend_title = NULL,
                                zero_intercept = TRUE, zero_axes = TRUE,
                                line_size = 0.5, marker_size = line_size * 4,
                                err_bar_size = line_size / 1.5,
                                err_bar_width = marker_size * 1.5) {

  if (class(df_summary) == "data.frame") {

    colnames(df_summary) <- c("Voltage", "SEMV", "Current", "SEMC")
    p <- ggplot(data = df_summary, mapping = aes(x = Voltage, y = Current))
    p <- p + geom_line(size = line_size)

    p <- p + geom_errorbar(mapping = aes(ymin = Current - SEMC, ymax = Current + SEMC),
                           size = err_bar_size, width = err_bar_width)

    p <- p + geom_point(size = marker_size)

    if (!zero_axes && zero_intercept) {
      p <- p + geom_vline(xintercept = 0, linetype = "dashed") +
        geom_hline(yintercept = 0, linetype = "dashed")
    }

    if (!is.null(title)) {
      p <- p + ggtitle(title)
    }

    p <- p + theme_classic()
    if (zero_axes) {
      p <- p + ZeroAxes(xlimit = df_summary[, 1], ylimit = df_summary[, 3],
                        xlabel = "Voltage", ylabel = "Current")
    }

    return(p)
  } else {

    df_melted <- NULL
    for (i in seq_along(df_summary)) {
      df <- cbind(names(df_summary)[i], df_summary[[i]])
      colnames(df)[1] <- "id"
      df_melted <- rbind(df_melted, df)
    }
    colnames(df_melted) <- c("id", "Voltage", "SEMV", "Current", "SEMC")

    p <- ggplot(data = df_melted, mapping = aes(x = Voltage, y = Current,
                                                colour = id, shape = id))
    p <- p + geom_line(size = line_size)
    p <- p + geom_errorbar(mapping = aes(ymin = Current - SEMC, ymax = Current + SEMC),
                           size = err_bar_size, width = err_bar_width)
    p <- p + geom_point(size = marker_size)

    if (!zero_axes && zero_intercept) {
      p <- p + geom_vline(xintercept = 0, linetype = "dashed") +
        geom_hline(yintercept = 0, linetype = "dashed")
    }

    p <- p + theme_classic()
    if (is.null(legend_title)) {
      p <- p + theme(legend.title = element_blank())
    } else {
      p <- p + labs(colour = as.character(legend_title),
                    shape = as.character(legend_title))
    }

    if (!is.null(title)) {
      p <- p + ggtitle(title)
    }


    if (zero_axes) {
      p <- p + ZeroAxes(xlimit = df_melted[, 2], ylimit = df_melted[, 4],
                        xlabel = "Voltage", ylabel = "Current")
    }

    return(p)
  }

}

