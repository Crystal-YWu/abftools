axis_label <- function(desc, unit, style = "%s (%s)") sprintf(style, desc, unit)

#' Draw axes centred at zero
#'
#' @details ZerosAxes() draw axes by annotating on (0, ) and (, 0). Since annotate()
#' is used, theme settings are not inherited, thus basesize must be set independently.
#'
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

#' Draw scale bars
#'
#' @param position Position to draw scale bars.
#' @param xrange Range of x axis.
#' @param yrange Range of y axis.
#' @param xscale Size of x scale. If is NULL, 1/20 of xrange.
#' @param yscale Size of y scale. If is NULL, 1/20 of yrange.
#' @param xlabel Label of x scale bar, if xscale is NULL, used as unit of x instead.
#' @param ylabel Label of y scale bar, if yscale is NULL, used as unit of x instead.
#' @param basesize Base textsize.
#' @param linesize Line size.
#'
#' @return a list of ggplot objects.
#' @export
#'
ScaleBars <- function(position = c("bl", "br", "tl", "tr"),
                      xrange, yrange, xscale = NULL, yscale = NULL, xlabel = "", ylabel = "",
                      basesize = 5, linesize = basesize / 4) {

  if (is.null(xscale)) {
    xscale <- (max(xrange) - min(xrange)) / 20
    xlabel <- sprintf("%.1f %s", xscale, xlabel)
  }
  if (is.null(yscale)) {
    yscale <- (max(yrange) - min(yrange)) / 20
    ylabel <- sprintf("%.1f %s", yscale, ylabel)
  }

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
