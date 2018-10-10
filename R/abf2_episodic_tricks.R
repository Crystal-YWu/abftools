#' FindSamplingInterval finds a stable interval for sampling current and voltage data of an abf object
#'
#' @param abf an abf object
#' @param current_chan_id OPTIONAL, channel id for current data
#' @param voltage_chan_id OPTIONAL, channel id for voltage data
#' @param min_sampling_size OPTIONAL, min size in points of a sampling interval
#' @param allowed_voltage_delta OPTIONAL, allowed max deviation of voltage W.R.T epoch per DAC setting
#' @param backward_search OPTIONAL, perform search along backward direction
#' @param strict_comp OPTIONAL, use strict compare mode, if set to FALSE, a fuzzy "mostly" compare is used instead
#'
#' @return a named vector of 3 numeric: interval start position, end position, length
#' @export
#'
#' @examples
FindSamplingInterval <- function(abf, current_chan_id = 0, voltage_chan_id = 0,
                                 min_sampling_size = 0, allowed_voltage_delta = 0,
                                 epoch_name = "auto", backward_search = TRUE,
                                 strict_comp = TRUE) {

  #figure out current channel and voltage channel
  if (current_chan_id == 0)
    current_chan_id <- GetFirstCurrentChan(abf)
  if (voltage_chan_id == 0)
    voltage_chan_id <- GetFirstVoltageChan(abf)
  if (is.na(current_chan_id))
    stop("FindSamplingInterval: Failed to find current channel id. Please provide manually.")
  if (is.na(voltage_chan_id))
    stop("FindSamplingInterval: Failed to find voltage channel id. Please provide manually.")

  if (epoch_name == "auto") {
    n_epoch <- nrow(abf$meta$EpochPerDAC)
    if (n_epoch > 3) {
      warning("FindSamplingInterval: Epoch B is automatically selected.")
    }
    epoch <- 2
  } else {
    epoch <- GetEpochId(epoch_name)
  }
  if (is.na(epoch)) {
    stop("FindSamplingInterval: Invalid epoch name.")
  }

  #Default allowed voltage delta is 5% of voltage epoch level increment
  if (allowed_voltage_delta == 0) {
    allowed_voltage_delta = abs(abf$meta$EpochPerDAC$fEpochLevelInc[epoch] * 0.05)
  }

  #Default minimal interval size is 10ms scan
  if (min_sampling_size == 0) {
    min_sampling_size <- floor(10.0 / abf$SampleInterval_ms)
  }
  if (min_sampling_size < 3) {
    min_sampling_size <- 3
  }

  #Collect available episodes
  epi <- colnames(abf[[voltage_chan_id]])
  #Calculate corresponding target voltage for each episode
  v <- sapply(epi, function(x) GetTargetVoltage(abf, epoch, x))
  #Find voltage windows that fit into allowed voltages
  intv <- GetAllowedWindows(abf[, 1, voltage_chan_id], v[1], allowed_voltage_delta,
                            min_sampling_size)
  for (i in seq(from = 2, to = length(v))) {
    win <- GetAllowedWindows(abf[, i, voltage_chan_id], v[i], allowed_voltage_delta,
                             min_sampling_size)
    intv <- GetWindowsOverlap(intv, win)
  }

  #TODO: exclude intervals that are in irrelevant epochs
  #TODO: parse EpochPerDAC

  if (nrow(intv) == 0) {
    s <- sprintf("FindSamplingInterval: No stable voltage interval found. Returning NA.
    allowed_voltage_delta = %.3f %s
    min_sampling_size = %d pts",
                 allowed_voltage_delta, abf$ChannelUnit[voltage_chan_id], min_sampling_size)
    warning(s)
    return(rep(NA, 3))
  }
  tmp <- FilterMinWindowSize(intv, min_sampling_size)
  if (nrow(tmp) == 0) {
    tmp <- FilterMinWindowSize(intv, 3)
    if (nrow(tmp) == 0) {
      s <- sprintf("FindSamplingInterval: No stable voltage interval found. Returning NA.
    allowed_voltage_delta = %.3f %s
    min_sampling_size = %d pts",
                   allowed_voltage_delta, abf$ChannelUnit[voltage_chan_id], min_sampling_size)
      warning(s)
      return(rep(NA, 3))
    }
    warning("FindSamplingInterval: Stable voltage intervals are smaller than min_sampling_size.")
  }
  #If failed to find voltage intervals larger than min_sampling_size, proceed
  #as is with those smaller intervals and throw a warning.
  intv <- tmp

  #function for comparing scores.
  f <- ifelse(strict_comp, all, mostly)
  best_score <- rep(Inf, length(epi))
  best_idx <- c()
  if (backward_search)
    itr <- seq(nrow(intv), 1)
  else
    itr <- seq_len(nrow(intv))
  for (i in itr) {
    mask <- seq(intv[i, 1], intv[i, 2])
    score <- colSds(abf[mask, , current_chan_id])
    if (f(score < best_score)) {
      best_score <- score
      best_idx <- i
    }
  }

  return(intv[best_idx, ])
}

#' ChannelIntervalMeans calculates interval means of a list of abf objects
#'
#' @param abf_list
#' @param intv_list
#' @param chan_id
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
ChannelIntervalMeans <- function(abf_list, intv_list, chan_id, na.rm = TRUE) {

  f <- function(x) colMeans(x, na.rm)

  return(ChannelInterval_f(abf_list, intv_list, chan_id, f))
}

#' ChannelIntervalSds calculates interval standard deviations of a list of abf objects
#'
#' @param abf_list
#' @param intv_list
#' @param chan_id
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
ChannelIntervalSds <- function(abf_list, intv_list, chan_id, na.rm = TRUE) {

  f <- function(x) colSds(x, na.rm)

  return(ChannelInterval_f(abf_list, intv_list, chan_id, f))
}

#' ChannelIntervalSems calculates interval SEM of a list of abf objects
#'
#' @param abf_list
#' @param intv_list
#' @param chan_id
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
ChannelIntervalSems <- function(abf_list, intv_list, chan_id, na.rm = TRUE) {

  f <- function(x) colSems(x, na.rm)

  return(ChannelInterval_f(abf_list, intv_list, chan_id, f))
}

#' AllSamples_IVSummary calculates average current of a list of abf objects, within given intervals
#'
#' @param abf_list
#' @param intv_list
#' @param current_chan_id
#' @param voltage_chan_id
#'
#' @return
#' @export
#'
#' @examples
AllSamples_IVSummary <- function(abf_list, intv_list, current_chan_id = 0,
                                 voltage_chan_id = 0) {

  #figure out current channel and voltage channel
  if (current_chan_id == 0)
    current_chan_id <- GetFirstCurrentChan(abf)
  if (voltage_chan_id == 0)
    voltage_chan_id <- GetFirstVoltageChan(abf)
  if (is.na(current_chan_id))
    stop("AllSamples_IVSummary: Failed to find current channel id. Please provide manually.")
  if (is.na(voltage_chan_id))
    stop("AllSamples_IVSummary: Failed to find voltage channel id. Please provide manually.")

  current_means <- ChannelIntervalMeans(abf_list, intv_list, current_chan_id)
  voltage_means <- ChannelIntervalMeans(abf_list, intv_list, voltage_chan_id)

  mean_current_means <- colMeans(current_means, na.rm = TRUE)
  mean_voltage_means <- colMeans(voltage_means, na.rm = TRUE)
  sem_current_means <- colSems(current_means, na.rm = TRUE)
  sem_voltage_means <- colSems(voltage_means, na.rm = TRUE)

  df <- data.frame(mean_voltage_means, sem_voltage_means, mean_current_means, sem_current_means)
  colnames(df) <- c("Voltage", "SEM Voltage", "Current", "SEM Current")
  return(df)
}

GetEpochId <- function(epoch_name) {

  epoch <- 0
  epoch_names <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
  epoch <- match(epoch_name, epoch_names)

  return(epoch)
}
GetFirstVoltageChan <- function(abf) match("Voltage", abf$ChannelNamePlot)
GetFirstCurrentChan <- function(abf) match("Current", abf$ChannelNamePlot)
GetTargetVoltage <- function(abf, epoch, epi) {

  init <- abf$meta$EpochPerDAC$fEpochInitLevel[epoch]
  incr <- abf$meta$EpochPerDAC$fEpochLevelInc[epoch]
  if (class(epi) == "character")
    epi <- as.integer(substring(epi, 4))
  else
    epi <- as.integer(epi)

  return(init + incr * (epi - 1))
}
GetAllowedWindows <- function(x, target, delta, min_window_size) {

  t_range <- c(target - abs(delta), target + abs(delta))
  v <- sapply(x, within_interval, t_range)
  r <- rle(v)
  idx_end <- cumsum(r$lengths)
  idx_start <- idx_end - r$lengths + 1
  idx_end <- idx_end[r$values]
  idx_start <- idx_start[r$values]
  win_length <- r$lengths[r$values]
  win <- cbind(idx_start, idx_end, win_length)

  return(FilterMinWindowSize(win, min_window_size))
}
GetWindowsOverlap <- function(w1, w2) {

  ret <- matrix(nrow = 0, ncol = 3)
  colnames(ret) <- colnames(w1)

  for (i in seq_len(nrow(w1))) {
    tmp1 <- w1[i, 1]:w1[i, 2]
    for (j in seq_len(nrow(w2))) {
      if (w2[j, 1] > w1[i, 2])
        break
      tmp2 <- w2[j, 1]:w2[j, 2]
      tmp <- intersect(tmp1, tmp2)
      if (length(tmp) != 0)
        ret <- rbind(ret, c(tmp[1], tmp[length(tmp)], length(tmp)))
    }
  }

  return(ret)
}
FilterMinWindowSize <- function(windows, min_window_size) {

  mask <- windows[, 3] >= min_window_size

  return(windows[mask, , drop = FALSE])
}
FilterMaxWindowSize <- function(windows, max_window_size) {

  mask <- windows[, 3] <= max_window_size

  return(windows[mask, , drop = FALSE])
}
ChannelInterval_f <- function(abf_list, intv_list, chan_id, f) {

  n <- length(abf_list)
  nepi <- ncol(abf_list[[1]][, , channel = chan_id])

  m <- matrix(NA, nrow = n, ncol = nepi)
  colnames(m) <- colnames(abf_list[[1]][, , channel = chan_id])

  for (i in seq_along(abf_list)) {
    if (any(is.na(intv_list[[i]]))) {
      next
    } else {
      mask <- seq(intv_list[[i]][1], intv_list[[i]][2])
      ret <- f(abf_list[[i]][mask, ,channel = chan_id])
      for (j in seq(nepi))
        m[i, j] <- ret[j]
    }
  }

  return(m)
}

#Is this possible to optimise?
within_interval <- function(x, intv) intv[1] <= x && x <= intv[2]
mostly <- function(x) {
  n <- sum(as.logical(x))
  return(n * 2L > length(x))
}
