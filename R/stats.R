#' IVSummary calculates average current of a list of abf objects, within given intervals
#'
#' @param abf_list a list of abf objects.
#' @param intv_list OPTIONAL, a list of intervals.
#' @param current_channel current channel id, 1-based.
#' @param voltage_channel voltage channel id, 1-based.
#'
#' @return a data frame containing calculated Voltage, SEM Voltage, Current, SEM Current columns.
#' @export
#'
IVSummary <- function(abf_list, intv_list, current_channel, voltage_channel) {

  if (!IsAbfList(abf_list)) {
    err_class_abf_list()
  }
  if (missing(intv_list) || is.null(intv_list)) {
    intv_list = list()
    for (i in seq_along(abf_list)) {
      intv_list[[i]] <- Intv(1L, nPts(abf_list[[i]]))
    }
  } else {
    intv_list <- ExpandList(intv_list, abf_list)
    if (is.null(intv_list)) {
      err_assert_len(intv_list, abf_list)
    }
  }
  #figure out current channel and voltage channel
  if (missing(current_channel) || is.null(current_channel)) {
    current_channel <- GetFirstCurrentChan(abf_list)
  }
  if (missing(voltage_channel) || is.null(voltage_channel)) {
    voltage_channel <- GetFirstVoltageChan(abf_list)
  }
  if (is.na(current_channel)) {
    err_id_current_chan()
  }
  if (is.na(voltage_channel)) {
    err_id_voltage_chan()
  }

  current_means <- MultiIntervalMeans(abf_list, intv_list, current_channel, na.rm = TRUE)
  voltage_means <- MultiIntervalMeans(abf_list, intv_list, voltage_channel, na.rm = TRUE)

  mean_current_means <- colMeans(current_means, na.rm = TRUE)
  mean_voltage_means <- colMeans(voltage_means, na.rm = TRUE)
  sem_current_means <- colSems(current_means, na.rm = TRUE)
  sem_voltage_means <- colSems(voltage_means, na.rm = TRUE)

  nsamples <- rep(length(abf_list), length(mean_voltage_means))

  df <- data.frame(mean_voltage_means, sem_voltage_means, mean_current_means,
                   sem_current_means, nsamples)
  colnames(df) <- c("Voltage", "SEM Voltage", "Current", "SEM Current", "Num Samples")
  return(df)
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

  if (!IsAbf(abf)) {
    err_class_abf()
  }

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

  old_samp_intv <- GetSamplingIntv(abf)
  new_samp_intv <- old_samp_intv * sampling_ratio

  #copy meta
  attr(data, "class") <- "abf"
  attr(data, "title") <- GetTitle(abf)
  attr(data, "mode") <- GetMode(abf)
  attr(data, "ChannelName") <- GetChannelName(abf)
  attr(data, "ChannelUnit") <- GetChannelUnit(abf)
  attr(data, "ChannelDesc") <- GetChannelDesc(abf)
  attr(data, "SamplingInterval") <- new_samp_intv
  attr(data, "EpiAvail") <- attr(abf, "EpiAvail")

  #alter meta
  meta <- get_meta(abf)
  meta$Protocol$fADCSequenceInterval <- new_samp_intv
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

  if (IsAbf(abf)) {
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

#' Calculate mean values of multiple abf objects
#'
#' @param abf_list a list of abf objects.
#' @param intv_list OPTIONAL, a list of intervals.
#' @param channel channel id, 1-based.
#' @param ret.df whether to return a data.frame object, if set to FALSE a matrix is returned instead.
#' @param na.rm wheter to remove na values..
#'
#' @return A data.frame object.
#' @export
#'
MultiMean <- function(abf_list, intv_list = NULL, channel = 1L, ret.df = TRUE,
                      na.rm = TRUE) {

  if (!IsAbfList(abf_list)) {
    err_class_abf_list()
  }
  intv_list <- ExpandList(intv_list, abf_list)
  if (is.null(intv_list)) {
    err_assert_len(intv_list, abf_list)
  }
  channel <- FirstElement(channel)
  for (tmp in abf_list) {
    if (!AssertChannel(tmp, channel)) {
      err_channel()
    }
  }

  colname <- c()
  for (i in seq_along(abf_list)) {
    colname[i] <- GetTitle(abf_list[[i]])
  }
  ret <- t(MultiIntervalMeans(abf_list, intv_list, channel, na.rm))
  colnames(ret) <- colname

  if (ret.df) {
    ret <- as.data.frame(ret)
  }
  return(ret)
}

#' Calculate mean currents of multiple abf objects
#'
#' @param abf_list a list of abf objects.
#' @param intv_list OPTIONAL, a list of intervals.
#' @param ret.df whether to return a data.frame object, if set to FALSE a matrix is returned instead.
#' @param na.rm wheter to remove na values..
#'
#' @return A data.frame object.
#' @export
#'
MultiMean_Current <- function(abf_list, intv_list = NULL, ret.df = TRUE, na.rm = TRUE) {

  channel <- GetFirstCurrentChan(abf_list)

  return(MultiMean(abf_list, intv_list, channel, ret.df, na.rm))
}

#' Calculate mean voltages of multiple abf objects
#'
#' @param abf_list a list of abf objects.
#' @param intv_list OPTIONAL, a list of intervals.
#' @param ret.df whether to return a data.frame object, if set to FALSE a matrix is returned instead.
#' @param na.rm wheter to remove na values.
#'
#' @return A data.frame object.
#' @export
#'
MultiMean_Voltage <- function(abf_list, intv_list = NULL, ret.df = TRUE, na.rm =TRUE) {

  channel <- GetFirstVoltageChan(abf_list)

  return(MultiMean(abf_list, intv_list, channel, ret.df, na.rm))
}


#' Calculate mean values of an abf object.
#'
#' Returns a matrix/data.frame containing episodic mean values of each channel.
#'
#' @param abf an abf object.
#' @param intv OPTIONAL, an interval to mean over.
#' @param ret.df whether to return a data.frame object, if set to FALSE a matrix is returned instead.
#' @param use_chan_name whether to use channel names as colnames, set to FALSE to use descriptive names instead.
#' @param na.rm whether to remove NA values.
#' @param ... passed to arithmetic mean.
#'
#' @return A matrix/data.frame.
#' @export
#' @method mean abf
#'
mean.abf <- function(abf, intv = NULL, ret.df = FALSE, use_chan_name = FALSE, na.rm = TRUE, ...) {

  if (use_chan_name) {
    chan_id_func <- GetChannelName
  } else {
    chan_id_func <- DefaultChanLabel
  }

  f <- WrapMappingFunc(mean, abf_id_func = NULL, epi_id_func = NULL,
                       chan_id_func = chan_id_func, ret.df = ret.df, na.rm = na.rm, ...)
  ret <- f(abf, intv)
  rownames(ret) <- DefaultEpiLabel(abf)

  return(ret)
}

#' Calculate standard deviation of an abf object.
#'
#' Returns a matrix/data.frame containing episodic sd of each channel.
#'
#' @param abf an abf object.
#' @param intv OPTIONAL, an interval to calculate.
#' @param ret.df whether to return a data.frame object, if set to FALSE a matrix is returned instead.
#' @param use_chan_name whether to use channel names as colnames, set to FALSE to use descriptive names instead.
#' @param na.rm whether to remove NA values.
#'
#' @return A matrix/data.frame.
#' @export
#'
sd_abf <- function(abf, intv = NULL, ret.df = FALSE, use_chan_name = FALSE, na.rm = TRUE) {

  if (use_chan_name) {
    chan_id_func <- GetChannelName
  } else {
    chan_id_func <- DefaultChanLabel
  }

  f <- WrapMappingFunc(stats::sd, abf_id_func = NULL, epi_id_func = NULL,
                       chan_id_func = chan_id_func, ret.df = ret.df, na.rm = na.rm)
  ret <- f(abf, intv)
  rownames(ret) <- DefaultEpiLabel(abf)

  return(ret)
}

sem_func <- function(x, na.rm) stats::sd(x, na.rm) / sqrt(length(x))
#' Calculate standard error of an abf object.
#'
#' Returns a matrix/data.frame containing episodic SEM of each channel.
#'
#' @param abf an abf object.
#' @param intv OPTIONAL, an interval to calculate.
#' @param ret.df whether to return a data.frame object, if set to FALSE a matrix is returned instead.
#' @param use_chan_name whether to use channel names as colnames, set to FALSE to use descriptive names instead.
#' @param na.rm whether to remove NA values.
#'
#' @return A matrix/data.frame.
#' @export
#'
sem_abf <- function(abf, intv = NULL, ret.df = FALSE, use_chan_name = FALSE, na.rm = TRUE) {

  if (use_chan_name) {
    chan_id_func <- GetChannelName
  } else {
    chan_id_func <- DefaultChanLabel
  }

  f <- WrapMappingFunc(sem_func, abf_id_func = NULL, epi_id_func = NULL,
                       chan_id_func = chan_id_func, ret.df = ret.df, na.rm = na.rm)
  ret <- f(abf, intv)
  rownames(ret) <- DefaultEpiLabel(abf)

  return(ret)
}

