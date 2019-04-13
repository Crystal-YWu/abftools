#' Crop undesired values from an abf object
#'
#' All values larger than max_value or smaller than min_value are replaced by NA
#' during the process, which helps better plotting and ruling out undesired features
#' such as sudden peak. Use with caution.
#'
#' @param abf an abf object.
#' @param channel the channel to crop values.
#' @param max_value OPTIONAL, max allowed value
#' @param min_value OPTIONAL, min allowed value
#'
#' @return an abf object with values cropped.
#' @export
#'
CropValue <- function(abf, channel, max_value, min_value) {

  CheckArgs(abf, chan = channel)

  for (ch in channel) {
    data <- abf[,, ch]
    if (!missing(max_value) && !is.null(max_value)) {
      data[data > max_value] = NA
    }
    if (!missing(min_value) && !is.null(min_value)) {
      data[data < min_value] = NA
    }
    abf[,, ch] <- data
  }

  abf
}

#' Get channel label for every channel of an abf object.
#'
#' @param abf an abf object
#' @param style a format string
#'
#' @return a vector of characters.
#' @export
#'
GetChanLabel <- function(abf, style) {

  if (!IsAbf(abf)) {
    err_class_abf()
  }

  GetAxisLabel(GetChannelDesc(abf), GetChannelUnit(abf), style = style)
}

#' Get default channel label for every channel of an abf object.
#'
#' @param abf an abf object
#'
#' @return a vector of characters.
#' @export
#'
DefaultChanLabel <- function(abf) {

  GetChanLabel(abf, style = "%s (%s)")
}

#' Get label for every episode of an abf object
#'
#' @param abf an abf object
#' @param style a format string
#'
#' @return a vector of characters.
#' @export
#'
GetEpiLabel <- function(abf, style) {

  if (IsAbf(abf)) {
    nep <- nEpi(abf)
  } else if (is.numeric(abf)) {
    nep <- as.integer(abf)
  } else {
    err_class_abf()
  }

  if (length(nep) == 1L) {
    sprintf(style, seq_len(nep))
  } else {
    sprintf(style, nep)
  }
}

#' Get default label for every episode of an abf object
#'
#' @param abf an abf object
#'
#' @return a vector of characters.
#' @export
#'
DefaultEpiLabel <- function(abf) {

  GetEpiLabel(abf, style = "epi%d")
}


#' Shift the axes of a ggplot object to 0
#'
#' @param xlimit limit of x values
#' @param ylimit limit of y values
#' @param xlabel label of x axis
#' @param ylabel label of y axis
#' @param xticks number of ticks of x axis
#' @param yticks number of ticks of y axis
#' @param textsize size of axis label size
#' @param ticksize size of tick labels
#'
#' @return a list of ggplot object
#' @export
#'
ZeroAxes <- function(xlimit, ylimit,
                     xlabel = NULL, ylabel = NULL,
                     xticks = 5, yticks = 5,
                     textsize = NULL, ticksize = NULL) {

  #remove xy axes
  theme_axis <- theme(axis.title.x = element_blank(),
                      axis.text.x = element_blank(),
                      axis.line.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.text.y = element_blank(),
                      axis.line.y = element_blank(),
                      axis.ticks.y = element_blank())

  x_axis <- geom_hline(yintercept = 0)

  #x_incr <- (max(xlimit) - min(xlimit)) / xticks
  #y_incr <- (max(ylimit) - min(ylimit)) / yticks

  x_tick <- pretty(c(min(xlimit), max(xlimit)), xticks)
  x_tick <- x_tick[x_tick >= min(xlimit)]
  x_tick <- x_tick[x_tick <= max(xlimit)]
  y_tick <- pretty(c(min(ylimit), max(ylimit)), yticks)
  y_tick <- y_tick[y_tick >= min(ylimit)]
  y_tick <- y_tick[y_tick <= max(ylimit)]

  xlabel_posx <- min(xlimit)# + x_incr
  xlabel_posy <- 0

  if (!is.null(xlabel)) {
    if (is.null(textsize)) {
      x_label <- annotate("text", x = xlabel_posx, y = xlabel_posy, label = xlabel,
                          hjust = 0, vjust = -1.5)
    } else {
      x_label <- annotate("text", x = xlabel_posx, y = xlabel_posy, label = xlabel,
                          size = textsize, hjust = 0, vjust = -1.5)
    }
  }
  #shape 3 hack
  x_pts <- annotate("point", x = x_tick, y = rep(0, length(x_tick)), shape = 3)

  ylabel_posx <- 0
  ylabel_posy <- min(ylimit)# + y_incr
  if (!is.null(ylabel)) {
    if (is.null(textsize)) {
      y_label <- annotate("text", x = ylabel_posx, y = ylabel_posy, label = ylabel,
                          angle = 90, hjust = 0, vjust = -1.5)
    } else {
      y_label <- annotate("text", x = ylabel_posx, y = ylabel_posy, label = ylabel,
                          size = textsize, angle = 90, hjust = 0, vjust = -1.5)
    }
  }

  #shape 3 hack
  y_pts <- annotate("point", x = rep(0, length(y_tick)), y = y_tick, shape = 3)

  x_tick_lab <- NULL
  for (x in x_tick) {
    if (x == 0) {
      next
    }
    if (is.null(ticksize)) {
      tmp <- annotate("text", x = x, y = 0, label = as.character(x),
                      hjust = 0.5, vjust = 1.5)
    } else {
      tmp <- annotate("text", x = x, y = 0, label = as.character(x),
                      size = ticksize, hjust = 0.5, vjust = 1.5)
    }

    x_tick_lab <- c(x_tick_lab, tmp)
  }
  y_tick_lab <- NULL
  for (y in y_tick) {
    if (y == 0) {
      next
    }
    tmp <- annotate("text", x = 0, y = y, label = as.character(y),
                    angle = 90, hjust = 0.5, vjust = 1.5)
    y_tick_lab <- c(y_tick_lab, tmp)
  }

  y_axis <- geom_vline(xintercept = 0)

  list(theme_axis,
       x_axis,
       y_axis,
       x_label,
       y_label,
       x_pts,
       y_pts,
       x_tick_lab,
       y_tick_lab)
}
