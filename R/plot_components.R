axis_label <- function(desc, unit, style = "%s (%s)") sprintf(style, desc, unit)

#' Draw axes centred at zero
#'
#' @details ZerosAxes() draw axes by annotating on (0, ) and (, 0). Since annotate()
#' is used, theme settings are not inherited, thus basesize must be set independently.
#'
#' @param p A ggplot object.
#' @param xrange Range of x axis.
#' @param yrange Range of y axis.
#' @param xlabel Label of x axis.
#' @param ylabel Label of y axis.
#' @param xticks Number of ticks on x axis.
#' @param yticks Number of ticks on y axis.
#' @param basesize Base textsize.
#'
#' @return a list of ggplot objects.
#' @export
#'
ZeroAxes <- function(xrange, yrange, xlabel, ylabel,
                     xticks = 5, yticks = 5, basesize= 5) {

  x_axis <- geom_hline(yintercept = 0)
  y_axis <- geom_vline(xintercept = 0)

  x_tick <- pretty(c(min(xrange), max(xrange)), xticks)
  x_tick <- x_tick[x_tick >= min(xrange)]
  x_tick <- x_tick[x_tick <= max(xrange)]
  y_tick <- pretty(c(min(yrange), max(yrange)), yticks)
  y_tick <- y_tick[y_tick >= min(yrange)]
  y_tick <- y_tick[y_tick <= max(yrange)]


  x_label <- annotate("text", x = min(xrange), y = 0, label = xlabel,
                      hjust = 0, vjust = -1.5, size = basesize)
  x_tick_pts <- annotate("point", x = x_tick, y = rep(0, length(x_tick)), shape = 3)
  x_tick_lab <- NULL
  for (x in x_tick) {
    if (x == 0) {
      next
    }
    x_tick_lab <- c(x_tick_lab, annotate("text", x = x, y = 0, label = as.character(x),
                                         hjust = 0.5, vjust = 1.5, size = basesize))
  }

  y_label <- annotate("text", x = 0, y = min(yrange), label = ylabel,
                      angle = 90, hjust = 0, vjust = -1.5, size = basesize)
  y_tick_pts <- annotate("point", x = rep(0, length(y_tick)), y = y_tick, shape = 3)
  y_tick_lab <- NULL
  for (y in y_tick) {
    if (y == 0) {
      next
    }
    y_tick_lab <- c(y_tick_lab, annotate("text", x = 0, y = y, label = as.character(y),
                                         angle = 90, hjust = 0.5, vjust = 1.5, size = basesize))
  }

  list(
    x_axis,
    y_axis,
    x_label,
    y_label,
    x_tick_pts,
    y_tick_pts,
    x_tick_lab,
    y_tick_lab
  )
}

#' @rdname ZeroAxes
#' @export
#'
ApplyZeroAxes <- function(p, xticks = 5, yticks = 5, basesize = 5) {

  xrange <- range(rlang::eval_tidy(p$mapping$x, p$data))
  yrange <- range(rlang::eval_tidy(p$mapping$y, p$data))
  xlabel <- p$labels$x
  ylabel <- p$labels$y

  p + ZeroAxes(xrange = xrange, yrange = yrange, xlabel = xlabel, ylabel = ylabel,
               xticks = xticks, yticks = yticks, basesize = basesize)
}

#' Draw scale bars
#'
#' @param p a ggplot object.
#' @param position Position to draw scale bars.
#' @param xrange Range of x axis.
#' @param yrange Range of y axis.
#' @param xscale Size of x scale. If is NULL, 1/20 of xrange.
#' @param yscale Size of y scale. If is NULL, 1/20 of yrange.
#' @param xunit Label of x scale bar, if xscale is NULL, used as unit of x instead.
#' @param yunit Label of y scale bar, if yscale is NULL, used as unit of x instead.
#' @param label_format Format string for labeling.
#' @param basesize Base textsize.
#' @param linesize Line size.
#'
#' @return a list of ggplot objects.
#' @export
#'
ScaleBars <- function(position = c("bl", "br", "tl", "tr"), xrange, yrange,
                      xscale = NULL, yscale = NULL, xunit = "", yunit = "",
                      label_format = "%g %s", basesize = 5, linesize = basesize / 4) {

  if (is.null(xscale)) {
    xscale <- (max(xrange) - min(xrange)) / 20
  }
  xlabel <- sprintf("%g %s", xscale, xunit)
  if (is.null(yscale)) {
    yscale <- (max(yrange) - min(yrange)) / 20
  }
  ylabel <- sprintf("%g %s", yscale, yunit)

  if (is.numeric(position)) {
    x <- position[1]
    xend <- x + xscale
    y <- position[2]
    yend <- y + yscale
    x_label <- annotate("text", label = xlabel, x = x, y = y,
                        hjust = 0.0, vjust = 1.5, size = basesize)
    y_label <- annotate("text", label = ylabel, x = x, y = y,
                        angle = 90, hjust = 0.0, vjust = -0.5, size = basesize)
  } else {
    position <- match.arg(position)
    x <- switch(position,
                bl = min(xrange),
                br = max(xrange),
                tl = min(xrange),
                tr = max(xrange))
    y <- switch(position,
                bl = min(yrange),
                br = min(yrange),
                tl = max(yrange),
                tr = max(yrange))
    xend <- switch(position,
                   bl = x + xscale,
                   br = x - xscale,
                   tl = x + xscale,
                   tr = x - xscale)
    yend <- switch(position,
                   bl = y + yscale,
                   br = y + yscale,
                   tl = y - yscale,
                   tr = y - yscale)
    x_label <- switch(position,
                      bl = annotate("text", label = xlabel, x = x, y = y,
                                    hjust = 0.0, vjust = 1.5, size = basesize),
                      tl = annotate("text", label = xlabel, x = x, y = y,
                                    hjust = 0.0, vjust = -0.5, size = basesize),
                      br = annotate("text", label = xlabel, x = x, y = y,
                                    hjust = 1.0, vjust = 1.5, size = basesize),
                      tr = annotate("text", label = xlabel, x = x, y = y,
                                    hjust = 1.0, vjust = -0.5, size = basesize))
    y_label <- switch(position,
                      bl = annotate("text", label = ylabel, x = x, y = y,
                                    angle = 90, hjust = 0.0, vjust = -0.5, size = basesize),
                      tl = annotate("text", label = ylabel, x = x, y = y,
                                    angle = 90, hjust = 1.0, vjust = -0.5, size = basesize),
                      br = annotate("text", label = ylabel, x = x, y = y,
                                    angle = 90, hjust = 0.0, vjust = 1.5, size = basesize),
                      tr = annotate("text", label = ylabel, x = x, y = y,
                                    angle = 90, hjust = 1.0, vjust = 1.5, size = basesize))
  }

  x_scale <- annotate("segment", x = x, xend = xend, y = y, yend = y, size = linesize)
  y_scale <- annotate("segment", x = x, xend = x, y = y, yend = yend, size = linesize)

  list(
    x_scale,
    y_scale,
    x_label,
    y_label
  )
}

#' @rdname ScaleBars
#' @export
#'
ApplyScaleBars <- function(p, position = c("bl", "br", "tl", "tr"),
                           xscale = NULL, yscale = NULL, xunit = "", yunit = "",
                           label_format = "%g %s", basesize = 5, linesize = basesize / 4) {

  xrange <- range(rlang::eval_tidy(p$mapping$x, p$data))
  yrange <- range(rlang::eval_tidy(p$mapping$y, p$data))
  position <- match.arg(position)

  p + ScaleBars(position = position, xrange = xrange, yrange = yrange,
                xscale = xscale, yscale = yscale, xunit = xunit, yunit = yunit,
                label_format = label_format, basesize = basesize, linesize = linesize)
}

#' Force zooming into Y axis.
#'
#' @details When clippling is TRUE, data values are directly clipped (set to NAs).
#' This is a workaround when there are multiple measurements in different units/scales,
#' since I haven't figure out a way to set coords to different group of data in
#' a ggplot object.
#'
#' @param p A ggplot object.
#' @param xrange Zoomed range of x axis.
#' @param yrange Zoomed range of y axis. If NULL, ydata will be evaluated from xdata and ydata.
#' @param xdata Data of x.
#' @param ydata Data of y, indices correspond to xdata.
#' @param space Spacing factor.
#' @param clipping clip value from clipping_cols.
#' @param clipping_cols columns to clip.
#'
#' @return ylim
#' @export
#'
ForceZoomY <- function(xrange, yrange = NULL, xdata, ydata, space = 0.0125) {

  if (is.null(yrange)) {
    xmin <- min(xrange)
    xmax <- max(xrange)
    #slow
    idx <- which(xmin < xdata & xdata < xmax)
    yrange <- range(ydata[idx])
    delta <- abs(yrange[2] - yrange[1])
    yrange[1] <- yrange[1] - delta * space
    yrange[2] <- yrange[2] + delta * space
  } else {
    yrange <- range(yrange)
  }

  ggplot2::coord_cartesian(ylim = yrange)
}

#' @rdname ForceZoomY
#' @export
#'
ApplyForceZoomY <- function(p, xrange, yrange = NULL, space = 0.0125, clipping = FALSE, clipping_cols = "value") {

  if (is.null(yrange)) {
    xdata <- rlang::eval_tidy(p$mapping$x, p$data)
    ydata <- rlang::eval_tidy(p$mapping$y, p$data)
    #evaluate yrange
    xmin <- min(xrange)
    xmax <- max(xrange)
    #slow
    idx <- which(xmin < xdata & xdata < xmax)
    yrange <- range(ydata[idx])
    delta <- abs(yrange[2] - yrange[1])
    yrange[1] <- yrange[1] - delta * space
    yrange[2] <- yrange[2] + delta * space
  }

  if (clipping) {
    #clip channel
    for (cols in clipping_cols) {
      idx <- p$data[[cols]] < yrange[1]
      p$data[[cols]][idx] <- NA
      idx <- p$data[[cols]] > yrange[2]
      p$data[[cols]][idx] <- NA
    }
    p
  } else {
    p + ForceZoomY(xrange = xrange, yrange = yrange,
                   xdata = NULL, ydata = NULL, space = space)
  }
}

#' Get default facet labeller for an abf object.
#'
#' @param abf an abf object.
#'
#' @return a labeller.
#' @export
#'
DefaultLabeller <- function(abf) {

  CheckArgs(abf)

  channel.labs <- DefaultChanLabel(abf)
  names(channel.labs) <- GetChanTag(GetAllChannels(abf))

  episode.labs <- DefaultEpiLabel(abf)
  names(episode.labs) <- GetEpiTag(GetAllEpisodes(abf))

  ggplot2::labeller(Channel = channel.labs, Episode = episode.labs)
}

#' @rdname ApplyCursor
#' @export
#'
CursorX <- function(cursor, colour = "black") {

  geom_vline(xintercept = cursor, linetype = "dashed", colour = colour)
}

#' @rdname ApplyCursor
#' @export
#'
CursorY <- function(cursor, colour = "black") {

  geom_hline(yintercept = cursor, linetype = "dashed", colour = colour)
}

#' @rdname ApplyCursor
#' @export
#'
ZeroCursor <- function(colour = "black") {

  list(
    CursorX(cursor = 0.0, colour = colour),
    CursorY(cursor = 0.0, colour = colour)
  )
}

#' @rdname ApplyCursor
#' @export
#'
ApplyZeroCursor <- function(p, colour = "black") {

  p + ZeroCursor(colour = colour)
}

#' Apply indicative cursors on plot.
#'
#' @param p a ggplot object
#' @param cursor a numeric vector.
#' @param cursor_x a numeric vector.
#' @param cursor_y a numeric vector.
#'
#' @return a ggplot object.
#' @export
#'
ApplyCursor <- function(p, cursor_x = NULL, cursor_y = NULL, colour = "black") {

  if (!is.null(cursor_x)) {
    p <- p + CursorX(cursor = cursor_x, colour = colour)
  }
  if (!is.null(cursor_y)) {
    p <- p + CursorY(cursor = cursor_y, colour = colour)
  }

  p
}
