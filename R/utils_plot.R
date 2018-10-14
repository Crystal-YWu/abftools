UniYLim <- function(abf_list, channel, intv_list = NULL) {

  ret <- c(0, 0)
  for (i in seq_along(abf_list)) {
    if (is.null(intv_list)) {
      lower <- min(abf_list[[i]][[channel]])
      upper <- max(abf_list[[i]][[channel]])
      delta <- abs(lower - upper)
      lower <- lower - delta * 0.0125
      upper <- upper + delta * 0.0125
    } else {
      focus <- (intv_list[[i]][1] + intv_list[[i]][2]) %/% 2L
      val <- sort(as.vector(abf_list[[i]][channel, focus, ]))
      lower <- val[1] - abs(val[1] - val[2]) * 0.5
      n <- length(val)
      upper <- val[n] + abs(val[n] - val[n - 1]) * 0.5
    }

    ret <- range(ret, lower, upper)
  }

  return(ret)
}

#' Melt channel data of an abf object into a data frame by sampling ratio.
#'
#' For easier melting abf channel data, sampling time (by given time unit) is
#' selected as id.vars, variable.name is set to Episode and value.name is
#' determined by your selected channel, which is extracted from ChannelDesc attr
#' of the abf object.
#'
#' You can also provide sampling_ratio and sampling_func to reduce data points
#' of the molten data frame. A sampling function should take a numeric vector as
#' input and return a single value or be left NULL so the data is sampled at the
#' exact position every sampling_ratio points.
#'
#' @param abf an abf object
#' @param channel the channel to melt, default to 1
#' @param sampling_ratio sampling ratio, default to 1 so that no sampling at all
#' @param sampling_func a function, default to null so no processing sampled points
#' @param time_unit time unit of the sampled data, can be tick, us, ms or s
#'
#' @return
#' @export
#'
#' @examples
#' Melt first channel with all default behaviors:
#' df <- melt(abf)
#' Melt second channel, sample every 200 points and take their mean, using time unit of ms:
#' df <- melt(abf, channel = 2, sampling_ratio = 200, sampling_func = mean, time_unit = "ms")
melt.abf <- function(abf, channel = 1L, sampling_ratio = 1L, sampling_func = NULL,
                     time_unit = "tick") {

  d <- dim(abf)
  n <- d[2]
  nepi <- d[3]
  chan_desc <- attr(abf, "ChannelDesc")[channel]

  data <- as.data.frame(abf, channel)
  ctick <- seq(from = 1, to = n, by = sampling_ratio)
  ctime <- TickToTime(abf, time_unit, ctick)
  #select rows by ticks
  df <- data[ctick, ]
  if ((sampling_ratio > 1L) && !is.null(sampling_func)) {
    #apply sampling function
    for (i in 1:(length(ctick) - 1)) {
      mask <- ctick[i]:(ctick[i + 1] - 1)
      sampling_value <- sapply(seq(nepi), function(x) sampling_func(data[mask, x]))
      df[i, ] <- sampling_value
    }
    i <- length(ctick)
    mask <- ctick[i]:n
    sampling_value <- sapply(seq(nepi), function(x) sampling_func(data[mask, x]))
    df[i, ] <- sampling_value
  }
  #bind time column
  df <- cbind(time = ctime, df)
  df <- melt(df, id.vars = "time", variable.name = "Episode", value.name = chan_desc)

  return(df)
}

TickToTime <- function(abf, time_unit, ctick) {

  ctime <- switch(time_unit,
                  tick = ctick,
                  us = ctick * attr(abf, "SamplingInterval"),
                  ms = ctick * attr(abf, "SamplingInterval") / 1000,
                  s  = ctick * attr(abf, "SamplingInterval") / 1000 / 1000,
                  stop("Convert sampling tick to time scale: time_unit can only be tick, us, ms or s."))

  return(ctime)
}