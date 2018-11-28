#' Quick plot I-V curves at given position
#'
#' @param abf an abf or a list of abf objects.
#' @param pos an interval/cursor or a list of intervals/cursors.
#' @param colour whether to plot in coloured mode.
#' @param title OPTIONAL, title for the plot.
#' @param zero_intercept whether to add zero intercepts to the plot.
#' @param zero_axes whether to plot axes at 0.
#'
#' @return a ggplot object.
#' @export
#'
QuickPlotIV <- function(abf, pos, colour = FALSE, title = NULL, legend_title = NULL,
                        zero_intercept = TRUE, zero_axes = TRUE) {

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
    p <- p + geom_line(aes_string(colour = "id"))
    if (is.null(legend_title)) {
      p <- p + theme(legend.title = element_blank())
    } else {
      p <- p + labs(`colour` = as.character(legend_title))
    }

  } else {
    p <- p + geom_line(aes_string(group = "id"))
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
#' @param err_bar_width width of the error bar
#' @param title OPTIONAL, title of the plot
#' @param legend_title OPTIONAL, if multiple IV summaries are to plot, title of the legend.
#' @param zero_intercept whether to add zero intercepts to the plot.
#' @param zero_axes whether to plot axes at 0.
#'
#' @return a ggplot object.
#' @export
#'
QuickPlot_IVSummary <- function(df_summary, err_bar_width = 1.5, title = NULL, legend_title = NULL,
                                zero_intercept = TRUE, zero_axes = TRUE) {

  if (class(df_summary) == "data.frame") {

    colnames(df_summary) <- c("Voltage", "SEMVoltage", "Current", "SEMCurrent")
    p <- ggplot(data = df_summary, mapping = aes(x = Voltage, y = Current, group = 1))
    p <- p + geom_line()
    p <- p + geom_errorbar(mapping = aes(ymin = Current - SEMCurrent, ymax = Current + SEMCurrent),
                           width = err_bar_width)
    p <- p + geom_point()
    if (zero_intercept) {
      p <- p + geom_vline(xintercept = 0, linetype = "dashed") +
        geom_hline(yintercept = 0, linetype = "dashed")
    }

    p <- p + theme_classic()

    if (!is.null(title)) {
      p <- p + ggtitle(title)
    }

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

    p <- ggplot(data = df_melted, mapping = aes_string(x = "Voltage", y = "Current", color = "id"))
    p <- p + geom_line()
    p <- p + geom_errorbar(mapping = aes(ymin = Current - SEMC, ymax = Current + SEMC),
                           width = err_bar_width)
    p <- p + geom_point()
    if (!zero_axes && zero_intercept) {
      p <- p + geom_vline(xintercept = 0, linetype = "dashed") +
        geom_hline(yintercept = 0, linetype = "dashed")
    }

    p <- p + theme_classic()
    if (is.null(legend_title)) {
      p <- p + theme(legend.title = element_blank())
    } else {
      p <- p + labs(`colour` = as.character(legend_title))
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

