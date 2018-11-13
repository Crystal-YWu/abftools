#' IVSummary calculates average current of a list of abf objects, within given intervals
#'
#' @param abf_list a list of abf objects.
#' @param intv_list a list of intervals.
#' @param current_channel current channel id, 1-based.
#' @param voltage_channel voltage channel id, 1-based.
#'
#' @return a data frame containing calculated Voltage, SEM Voltage, Current, SEM Current columns.
#' @export
#'
IVSummary <- function(abf_list, intv_list, current_channel, voltage_channel) {

  #figure out current channel and voltage channel
  if (missing(current_channel) || is.null(current_channel)) {
    current_channel <- GetFirstCurrentChan(abf_list[[1]])
  }
  if (missing(voltage_channel) || is.null(voltage_channel)) {
    voltage_channel <- GetFirstVoltageChan(abf_list[[1]])
  }
  if (is.na(current_channel)) {
    err_id_current_chan()
  }
  if (is.na(voltage_channel)) {
    err_id_voltage_chan()
  }

  current_means <- EpisodicIntervalMeans(abf_list, intv_list, current_channel)
  voltage_means <- EpisodicIntervalMeans(abf_list, intv_list, voltage_channel)

  mean_current_means <- colMeans(current_means, na.rm = TRUE)
  mean_voltage_means <- colMeans(voltage_means, na.rm = TRUE)
  sem_current_means <- colSems(current_means, na.rm = TRUE)
  sem_voltage_means <- colSems(voltage_means, na.rm = TRUE)

  df <- data.frame(mean_voltage_means, sem_voltage_means, mean_current_means, sem_current_means)
  colnames(df) <- c("Voltage", "SEM Voltage", "Current", "SEM Current")
  return(df)
}

#' Average a list of abf objects.
#'
#' @param abf_list a list of abf objects.
#' @param w a vector of weights for weighted average, leave 0 for arithmetic mean.
#'
#' @return an averaged abf object, of which the protocol settings follow first element in abf_list.
#' @export
#'
AverageAbf <- function(abf_list, w) {

  if (!IsAbfList(abf_list)) {
    err_class_abf_list()
  }
  if (class(abf_list) == "abf") {
    return(abf_list)
  }

  if (missing(w) || is.null(w)) {
    n <- length(abf_list)
    ret <- abf_list[[1]]
    for (i in 2:n) {
      ret <- ret + abf_list[[i]]
    }
    ret <- ret / n
  } else {
    n <- length(abf_list)
    if (n != length(w)) {
      err_wrong_dim()
    }
    ret <- abf_list[[1]] * w[1]
    for (i in 2:n) {
      ret <- ret + abf_list[[i]] * w[i]
    }
    ret <- ret / sum(w)
  }


  return(ret)
}

#' Sample abf object to reduce data points.
#'
#' @param abf an abf object.
#' @param sampling_ratio the sampling ratio. See melt.abf for more details.
#' @param sampling_func a sampling function applied to sampled points. See melt.abf for more details.
#'
#' @return a sampled abf object
#' @export
#'
SmplAbf <- function(abf, sampling_ratio, sampling_func = NULL) {

  nch <- nChan(abf)
  nepi <- nEpi(abf)
  npts_abf <- nPts(abf)

  #indices of the sampled points
  idx_smpl <- seq(from = 1L, to = npts_abf, by = sampling_ratio)
  #npts for sampled abf
  npts <- length(idx_smpl)
  #copy sampled points to data
  data <- array(abf[idx_smpl, , ], dim = c(npts, nepi, nch))
  #TODO: this is super SLOW, optimisation needed.
  #Solution 1. restrict sampling_func to a function that applies data by column,
  #and provide high performance colXX functions to users. Should consider
  #consistency with sampling_func of melt.abf
  #Solution 2. convert this part to Rcpp.
  if (!is.null(sampling_func)) {
    for (chan in seq_len(nch)) {
      #for (epi in seq.int(nepi)) {
      #  for (i in seq.int(npts - 1L)) {
      #    mask <- seq.int(idx_smpl[i], idx_smpl[i + 1] - 1L)
      #    data[i, epi, chan] <- sampling_func(abf[mask, epi, chan])
      #  }
      #  mask <- seq.int(idx_smpl[npts], npts_abf)
      #  data[npts, epi, chan] <- sampling_func(abf[mask, epi, chan])
      #}
      #sapply sees speed degrade. use loop instead.
      #for (i in seq.int(npts - 1L)) {
      #  mask <- seq.int(idx_smpl[i], idx_smpl[i + 1] - 1L)
      #  tmp_values <- sapply(seq.int(nepi), function(x) sampling_func(abf[mask, x, chan]))
      #  data[i, , chan] <- tmp_values
      #}
      #mask <- seq.int(idx_smpl[npts], npts_abf)
      #tmp_values <- sapply(seq.int(nepi), function(x) sampling_func(abf[mask, x, chan]))
      #data[npts, , chan] <- tmp_values
      for (i in seq_len(npts - 1L)) {
        mask <- seq(idx_smpl[i], idx_smpl[i + 1] - 1L)
        data[i, , chan] <- sampling_func(abf[mask, , chan])
      }
      mask <- seq(idx_smpl[npts], npts_abf)
      data[npts, , chan] <- sampling_func(abf[mask, , chan])
    }
  }

  #copy meta
  attr(data, "class") <- "abf"
  attr(data, "title") <- GetTitle(abf)
  attr(data, "mode") <- GetMode(abf)
  attr(data, "ChannelName") <- GetChannelName(abf)
  attr(data, "ChannelUnit") <- GetChannelUnit(abf)
  attr(data, "ChannelDesc") <- GetChannelDesc(abf)
  attr(data, "SamplingInterval") <- GetSamplingIntv(abf)
  attr(data, "EpiAvail") <- attr(abf, "EpiAvail")

  #alter meta
  meta <- get_meta(abf)
  meta$Protocol$lNumSamplesPerEpisode <- npts * nch
  attr(data, "meta") <- meta

  return(data)
}

#' Sample abf object to reduce data points, by-ref like behaviour.
#'
#' @param abf an abf object.
#' @param sampling_ratio the sampling ratio. See melt.abf for more details.
#' @param sampling_func a sampling function applied to sampled points. See melt.abf for more details.
#'
#' @return the sampled abf itself
#' @export
#'
SampleAbf <- function(abf, sampling_ratio, sampling_func = NULL) {

  if (class(abf) == "abf") {
    return(
      eval.parent(substitute({
        abf <- SmplAbf(abf, sampling_ratio, sampling_func)
      }))
    )
  } else if (IsAbfList(abf)) {
    return(
      eval.parent(substitute({
        abf <- lapply(abf, function(x) SmplAbf(x, sampling_ratio, sampling_func))
      }))
    )
  } else {
    err_class_abf_list()
  }
}

#' Calculate mean values of an abf object.
#'
#' Returns a data.frame object containing episodic mean values of each channel.
#'
#' @param abf an abf object.
#' @param intv OPTIONAL, an interval to mean over.
#' @param desc_colnames Whether to use descriptive colnames, set to FALSE to use original channel names.
#' @param ... Passed to arithmetic mean.
#'
#' @return A data.frame object.
#' @export
#' @method mean abf
#'
mean.abf <- function(abf, intv, desc_colnames = TRUE, ...) {

  if (missing(intv) || is.null(intv)) {
    intv <- c(1, nPts(abf), nPts(abf))
  }

  ncols <- nChan(abf)
  nrows <- nEpi(abf)
  ret <- matrix(nrow = nrows, ncol = ncols)
  mask <- MaskIntv(intv)
  for (i in seq_len(ncols))
    for (j in seq_len(nrows)) {
      ret[j, i] <- mean(abf[mask, j, i], ...)
    }
  if (desc_colnames) {
    colnames(ret) <- paste0(GetChannelDesc(abf), " (", GetChannelUnit(abf), ")")
  } else {
    colnames(ret) <- GetChannelName(abf)
  }

  return(as.data.frame(ret))
}

#' Calculate mean values of multiple abf objects
#'
#' @param abf_list a list of abf objects.
#' @param intv_list a list of intervals.
#' @param channel channel id, 1-based.
#' @param na.rm wheter to remove na values..
#'
#' @return A data.frame object.
#' @export
#'
MultiMean <- function(abf_list, intv_list, channel = 1, na.rm = TRUE) {

  colname <- c()
  for (i in seq_along(abf_list)) {
    colname[i] <- GetTitle(abf_list[[i]])
  }
  ret <- as.data.frame(t(EpisodicIntervalMeans(abf_list, intv_list, channel, na.rm)))
  colnames(ret) <- colname

  return(ret)
}

#' Calculate mean currents of multiple abf objects
#'
#' @param abf_list a list of abf objects.
#' @param intv_list a list of intervals.
#' @param na.rm wheter to remove na values..
#'
#' @return A data.frame object.
#' @export
#'
MultiMean_Current <- function(abf_list, intv_list, na.rm = TRUE) {

  channel <- GetFirstCurrentChan(abf_list[[1]])

  return(MultiMean(abf_list, intv_list, channel, na.rm))
}

#' Calculate mean voltages of multiple abf objects
#'
#' @param abf_list a list of abf objects.
#' @param intv_list a list of intervals.
#' @param na.rm wheter to remove na values.
#'
#' @return A data.frame object.
#' @export
#'
MultiMean_Voltage <- function(abf_list, intv_list, na.rm =TRUE) {

  channel <- GetFirstVoltageChan(abf_list[[1]])

  return(MultiMean(abf_list, intv_list, channel, na.rm))
}
