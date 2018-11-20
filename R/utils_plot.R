#' Melt channel data of an abf object into a data frame by sampling ratio.
#'
#' For easier melting abf channel data, sampling time (by given time unit) is
#' selected as id.vars, variable.name is set to Episode and value.name is
#' determined by your selected channel, which is extracted from ChannelDesc attr
#' of the abf object.
#'
#' You can also provide sampling_ratio and sampling_func to reduce data points
#' of the molten data frame. A sampling function should accept a matrix as input
#' and apply column-wise ops to the sampled data and return a numeric (e.g. colMeans),
#' or be left NULL so no sampling ops is applied at all.
#'
#' @param abf an abf object
#' @param channel the channel to melt, default to 1
#' @param sampling_ratio sampling ratio, default to 1 so that no sampling at all
#' @param sampling_func a function, default to null so no processing sampled points
#' @param time_unit time unit of the sampled data, can be tick, us, ms, s, min or hr
#' @param ... other arguments passed to melt
#' @param value.name OPTIONAL, name of the channel value, defaults to generated channel description.
#'
#' @return a melted data frame.
#' @export
#'
melt.abf <- function(abf, channel = 1L, sampling_ratio = 1L, sampling_func = NULL,
                     time_unit = "tick", ..., value.name = NULL) {

  if (!IsAbf(abf)) {
    err_class_abf()
  }
  if (!AssertChannel(abf, channel)) {
    err_channel()
  }

  npts <- nPts(abf)
  if (is.null(value.name)) {
    value.name <- GetChannelDesc(abf)[channel]
  } else {
    value.name <- as.character(value.name)
  }

  #extract channel data
  data <- as.data.frame(abf, channel = channel)
  #channel index tick
  ctick <- seq(from = 1L, to = npts, by = sampling_ratio)
  #convert to time
  ctime <- TickToTime(abf, time_unit, ctick)
  #sample data by sampling_ratio
  df <- data[ctick, , drop = FALSE]
  if ((sampling_ratio > 1L) && !is.null(sampling_func)) {
    #apply sampling function
    for (i in 1L:(length(ctick) - 1L)) {
      mask <- ctick[i]:(ctick[i + 1L] - 1L)
      #sampling_value <- sapply(seq.int(nepi), function(x) sampling_func(data[mask, x]))
      sampling_value <- sampling_func(data[mask, , drop = FALSE])
      df[i, ] <- sampling_value
    }
    i <- length(ctick)
    mask <- ctick[i]:npts
    #sampling_value <- sapply(seq.int(nepi), function(x) sampling_func(data[mask, x]))
    sampling_value <- sampling_func(data[mask, , drop = FALSE])
    df[i, ] <- sampling_value
  }
  #bind time column
  df <- cbind(time = ctime, df)
  df <- melt(df, id.vars = "time", variable.name = "Episode", value.name = value.name, ...)

  return(df)
}

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

  if (!IsAbf(abf)) {
    err_class_abf()
  }
  if (!AssertChannel(abf, channel)) {
    err_channel()
  }

  if (!missing(max_value) && !is.null(max_value)) {
    abf[abf > max_value] = NA
  }
  if (!missing(min_value) && !is.null(min_value)) {
    abf[abf < min_value] = NA
  }

  return(abf)
}

#' Get default channel label for every channel of an abf object.
#'
#' @param abf an abf object
#'
#' @return a vector of characters.
#' @export
#'
DefaultChanLabel <- function(abf) {

  if (!IsAbf(abf)) {
    err_class_abf()
  }

  return(GetAxisLabel(GetChannelDesc(abf), GetChannelUnit(abf)))
}

#' Get default label for every episode of an abf object
#'
#' @param abf an abf object
#'
#' @return a vector of characters.
#' @export
#'
DefaultEpiLabel <- function(abf) {

  if (IsAbf(abf)) {
    nep <- nEpi(abf)
  } else if (is.numeric(abf)) {
    nep <- as.integer(abf)
  } else {
    err_class_abf()
  }

  ret <- paste0("epi", seq_len(nep))

  return(ret)
}

