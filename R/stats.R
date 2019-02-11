#' IVSummary calculates average current of a list of abf objects, within given intervals
#'
#' @param abf_list a list of abf objects.
#' @param intv_list OPTIONAL, a list of intervals.
#' @param current_channel current channel id, 1-based.
#' @param voltage_channel voltage channel id, 1-based.
#'
#' @return a data frame containing calculated Voltage, SEM Voltage, Current,
#' SEM Current and number of samples.
#' @export
#'
IVSummary <- function(abf_list, intv_list = NULL,
                      current_channel = GetFirstCurrentChan(abf_list),
                      voltage_channel = GetFirstVoltageChan(abf_list)) {

  if (!IsAbfList(abf_list)) {
    err_class_abf_list()
  }
  CheckArgs(abf_list, chan = c(current_channel, voltage_channel), allow_list = TRUE)
  intv_list <- CheckIntvList(abf_list, intv_list)

  current_means <- Episodic_ColFunc(abf_list, intv_list, current_channel,
                                    colMeans, na.rm = TRUE)
  voltage_means <- Episodic_ColFunc(abf_list, intv_list, voltage_channel,
                                    colMeans, na.rm = TRUE)

  mean_current_means <- colMeans(current_means, na.rm = TRUE)
  mean_voltage_means <- colMeans(voltage_means, na.rm = TRUE)
  sem_current_means <- colSems(current_means, na.rm = TRUE)
  sem_voltage_means <- colSems(voltage_means, na.rm = TRUE)

  nsamples <- rep(length(abf_list), length(mean_voltage_means))

  df <- data.frame(mean_voltage_means, sem_voltage_means,
                   mean_current_means, sem_current_means, nsamples)
  colnames(df) <- c("Voltage", "SEM Voltage", "Current", "SEM Current", "Num Samples")

  CpChannelAttr(df, abf_list[[1]])
}

#' Sample abf object to reduce data points.
#'
#' @param abf an abf object.
#' @param sampling_ratio the sampling ratio. See melt.abf for more details.
#' @param sampling_func a sampling function applied to sampled points. See melt.abf for more details.
#' @param ... arguments passed to sampling_func
#'
#' @return a sampled abf object
#' @export
#'
SmplAbf <- function(abf, sampling_ratio, sampling_func = NULL, ...) {

  CheckArgs(abf)

  nch <- nChan(abf)
  nepi <- nEpi(abf)
  npts_abf <- nPts(abf)

  idx_smpl <- seq(from = 1L, to = npts_abf, by = sampling_ratio)
  npts <- length(idx_smpl)
  data <- array(abf[idx_smpl, , ], dim = c(npts, nepi, nch))

  if (!is.null(sampling_func)) {
    idx_end <- c(idx_smpl[2:npts] - 1L, npts_abf)
    if (GetMode(abf) == 1L) {
      warned <- FALSE
      for (ch in seq_len(nch)) {
        for (i in seq_len(npts)) {
          mask <- seq.int(idx_smpl[i], idx_end[i])
          tmp <- abf[mask, , ch]
          if (all(is.na(tmp))) {
            break
          }
          val <- sampling_func(tmp)
          nan <- which(is.nan(val))
          if (length(nan) && !warned) {
            val[nan] <- NA
            warning("NaN values are replaced by NAs.")
            warned <- TRUE
          }
          data[i, , ch] <- val
        }
      }
    } else {
      for (ch in seq_len(nch)) {
        for (i in seq_len(npts)) {
          mask <- seq.int(idx_smpl[i], idx_end[i])
          data[i, , ch] <- sampling_func(abf[mask, , ch])
        }
      }
    }
  }

  old_samp_intv <- GetSamplingIntv(abf)
  new_samp_intv <- old_samp_intv * sampling_ratio

  #copy meta
  CpAbfAttr(data, abf)
  attr(data, "SamplingInterval") <- new_samp_intv

  #alter meta
  meta <- get_meta(abf)
  meta$Protocol$fADCSequenceInterval <- new_samp_intv
  if (GetMode(abf) != 3L) {
    nepi <- nEpi(abf)
    for (i in seq_len(nepi)) {
      meta$SynchArray$lLength[i] <- sum(!is.na(data[, i, 1L])) * nch
    }
  }
  attr(data, "meta") <- meta

  data
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
  CheckArgs(abf_list, chan = channel, allow_list = TRUE)
  intv_list <- CheckIntvList(abf_list, intv_list)

  if (length(channel) == 1L) {
    ret <- t(Episodic_ColFunc(abf_list, intv_list, channel, colMeans, na.rm = na.rm))
    if (ret.df) {
      ret <- as.data.frame(ret)
    }
  } else {
    ret <- lapply(channel, function(x) t(Episodic_ColFunc(abf_list, intv_list,
                                                          channel = x, f = colMeans,
                                                          na.rm = na.rm)))
    if (ret.df) {
      ret <- lapply(ret, as.data.frame)
    }
    names(ret) <- GetChannelDesc(abf_list[[1]])[channel]
  }

  ret
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
  MultiMean(abf_list, intv_list, channel, ret.df, na.rm)
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
  MultiMean(abf_list, intv_list, channel, ret.df, na.rm)
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

  ret
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

  ret
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

  ret
}
