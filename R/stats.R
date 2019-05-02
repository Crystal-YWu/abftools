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
  intv_list <- ExpandIntvList(abf_list, intv_list)

  current_means <- Episodic_colFunc(abf_list, intv_list, current_channel,
                                    colMeans, na.rm = TRUE)
  voltage_means <- Episodic_colFunc(abf_list, intv_list, voltage_channel,
                                    colMeans, na.rm = TRUE)

  mean_current_means <- colMeans(current_means, na.rm = TRUE)
  mean_voltage_means <- colMeans(voltage_means, na.rm = TRUE)
  sem_current_means <- colSems(current_means, na.rm = TRUE)
  sem_voltage_means <- colSems(voltage_means, na.rm = TRUE)

  mean_current_means[is.nan(mean_current_means)] <- NA
  mean_voltage_means[is.nan(mean_voltage_means)] <- NA
  sem_current_means[is.nan(sem_current_means)] <- NA
  sem_voltage_means[is.nan(sem_voltage_means)] <- NA

  nsamples <- rep(length(abf_list), length(mean_voltage_means))

  df <- data.frame(mean_voltage_means, sem_voltage_means,
                   mean_current_means, sem_current_means, nsamples)
  colnames(df) <- c("Voltage", "SEM Voltage", "Current", "SEM Current", "Num Samples")

  ApplyChannelAttr(df, abf_list[[1]])
}

do_igv <- function(abf, intv, i_chan, v_chan) {

  force(i_chan)
  force(v_chan)

  if (anyNA(intv)) {
    d <- dim(abf)
    i <- rep(NA, d[2])
    v <- i
    g <- i
  } else {
    if (is.null(intv)) {
      iv <- mapnd_col(abf, matrixStats::colMeans2)
    } else {
      mask <- MaskIntv(intv)
      iv <- mapnd_col(abf[mask,,], matrixStats::colMeans2)
    }
    i <- iv[, i_chan]
    v <- iv[, v_chan]
    g <- slope_spline(i, v)
  }

  list(
    i = i,
    v = v,
    g = g
  )
}

IGVSummary <- function(abf, intv,
                       current_channel = GetFirstCurrentChan(abf),
                       voltage_channel = GetFirstVoltageChan(abf)) {

  CheckArgs(abf, chan = c(current_channel, voltage_channel), allow_list = TRUE)
  if (!IsAbfList(abf)) {
    abf <- list(abf)
  }
  if (!AssertDim(abf, 2)) {
    stop("Episodes of abf do not match.")
  }
  n <- length(abf)
  intv <- ExpandIntvList(abf, intv)

  igv <- lapply(seq_len(n), function(idx) {
    do_igv(abf[[idx]], intv[[idx]], i_chan = current_channel, v_chan = voltage_channel)
  })
  igv <- mapply(do_igv, abf, intv,
                MoreArgs = list(i_chan = current_channel,
                                v_chan = voltage_channel),
                SIMPLIFY = FALSE,
                USE.NAMES = FALSE)

  i <- sapply(igv, `[[`, "i")
  g <- sapply(igv, `[[`, "g")
  v <- sapply(igv, `[[`, "v")

  #averaging conductance and calculating its SEM directly is very rigorous.
  current <- matrixStats::rowMeans2(i, na.rm = TRUE)
  conduct <- matrixStats::rowMeans2(g, na.rm = TRUE)
  voltage <- matrixStats::rowMeans2(v, na.rm = TRUE)
  current_sem <- rowSems(i, na.rm = TRUE)
  conduct_sem <- rowSems(g, na.rm = TRUE)
  voltage_sem <- rowSems(v, na.rm = TRUE)

  df <- data.frame(voltage, voltage_sem, current, current_sem, conduct, conduct_sem, n)
  colnames(df) <- c("Voltage", "SEM Voltage",
                    "Current", "SEM Current",
                    "Conductance", "SEM Conductance",
                    "Num Samples")
  rownames(df) <- DefaultEpiLabel(nrow(df))

  df
}

#' Sample abf object to reduce data points.
#'
#' @param abf an abf object.
#' @param sample_ratio the sampling ratio. See melt.abf for more details.
#' @param sample_func a sampling function applied to sampled points.
#' @param ... arguments passed to sampling_colFunc
#'
#' @return a sampled abf object
#' @export
#'
SmplAbf <- function(abf, sample_ratio, sample_func = NULL, ...) {

  CheckArgs(abf)

  data <-  ApplyAbfAttr(x = samplend(abf,
                                     ratio = sample_ratio,
                                     func = sample_func,
                                     along = 1L, ...),
                        abf = abf)

  old_samp_intv <- GetSamplingIntv(abf)
  new_samp_intv <- old_samp_intv * sample_ratio
  attr(data, "SamplingInterval") <- new_samp_intv

  #alter meta
  meta <- get_meta(abf)
  meta$Protocol$fADCSequenceInterval <- new_samp_intv
  if (!is.null(meta$SynchArray)) {
    d <- dim(data)
    if (GetMode(abf) == 1L) {
      nepi <- d[2]
      data[which(is.nan(data))] <- NA
      for (event in seq_len(nepi)) {
        meta$SynchArray$lLength[event] <- sum(!is.na(data[, event, 1L]))
      }
    } else {
      meta$SynchArray$lLength <- d[1] %/% d[3]
    }
  }
  attr(data, "meta") <- meta

  data
}

#' Sample abf object to reduce data points, by-ref like behaviour.
#'
#' @param abf an abf object.
#' @param sample_ratio the sampling ratio. See melt.abf for more details.
#' @param sample_func a sampling function applied to sampled points.
#' @param ... further arguments passed to sampling_colFunc
#'
#' @return the sampled abf itself
#' @export
#'
SampleAbf <- function(abf, sample_ratio, sample_func = NULL, ...) {

  if (IsAbf(abf)) {
    return(
      eval.parent(substitute({
        abf <- SmplAbf(abf, sample_ratio, sample_func, ...)
      }))
    )
  } else if (IsAbfList(abf)) {
    return(
      eval.parent(substitute({
        abf <- lapply(abf, SmplAbf, sample_ratio = sample_ratio,
                      sample_func = sample_func, ...)
      }))
    )
  } else {
    err_class_abf()
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
  intv_list <- ExpandIntvList(abf_list, intv_list)

  if (length(channel) == 1L) {
    ret <- t(Episodic_colFunc(abf_list, intv_list, channel, colMeans, na.rm = na.rm))
    if (ret.df) {
      ret <- as.data.frame(ret)
    }
  } else {
    ret <- lapply(channel, function(x) t(Episodic_colFunc(abf_list, intv_list,
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

#' Average a list of abf objects.
#'
#' @param abf_list a list of abf objects.
#' @param w OPTIONAL, a vector of weights for weighted average.
#'
#' @return an averaged abf object, of which the protocol settings follow first element in abf_list.
#' @export
#'
AverageAbf <- function(abf_list, w = NULL) {

  if (!IsAbfList(abf_list)) {
    err_class_abf_list()
  }

  if (length(abf_list) == 1L) {
    return(abf_list[[1]])
  }

  if (is.null(w)) {
    n <- length(abf_list)
    ret <- abf_list[[1]]
    for (i in 2:n) {
      ret <- ret + abf_list[[i]]
    }
    ret <- ret / n
  } else {
    if (!AssertLength(w, abf_list)) {
      err_assert_len(w, abf_list)
    }

    n <- length(abf_list)
    ret <- abf_list[[1]] * w[1]
    for (i in 2:n) {
      ret <- ret + abf_list[[i]] * w[i]
    }
    ret <- ret / sum(w)
  }

  ret
}

ApplyBlank <- function(abf, chan, epi_val) {

  abf[,, chan] <- sweep(abf[,, chan], 2L, epi_val)
  abf
}

#' Blank current channel of abf objects.
#'
#' @param abf an abf object or a list of abf objects.
#' @param ref_data reference current data to blank or abf objects as reference.
#' @param ref_intv OPTIONAL, if ref_data is abf objects, a time interval to
#' calculate mean current.
#' @param current_channel OPTIONAL, the current channel to apply blanking,
#' if not given, the first current channel is used.
#'
#'
#' @return an abf object or a list of abf objects
#' @export
#'
BlankAbf <- function(abf, ref_data, ref_intv = NULL,
                     current_channel = GetFirstCurrentChan(abf)) {

  #reference data
  if (IsAbf(ref_data)) {
    ref_data <- mean(ref_data, intv = ref_intv)
  } else if (IsAbfList(ref_data)) {
    ref_data <- IVSummary(ref_data, ref_intv, current_channel = current_channel)
  }
  current_data <- ParseDataFrameIV(ref_data)$Current
  idx_na <- is.na(current_data)
  if (all(idx_na)) {
    err_channel_data("Current")
  } else {
    current_data[which(idx_na)] <- 0.0
  }

  if (IsAbf(abf)) {
    abf <- ApplyBlank(abf, current_channel, current_data)
  } else if (IsAbfList(abf)) {
    abf <- lapply(abf, ApplyBlank, current_channel, current_data)
  }

  abf
}
