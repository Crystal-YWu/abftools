#' FindSamplingInterval finds a stable interval for sampling current and voltage data of an abf object
#'
#' @param abf an abf object
#' @param current_chan_id OPTIONAL, channel id for current data
#' @param voltage_chan_id OPTIONAL, channel id for voltage data
#' @param min_sampling_size OPTIONAL, min size in points of a sampling interval
#' @param allowed_voltage_delta OPTIONAL, allowed max deviation of voltage W.R.T epoch per DAC setting
#' @param backward_search OPTIONAL, perform search along backward direction
#' @param strict_comp OPTIONAL, use strict compare mode, if set to FALSE, a fuzzy "mostly" compare is used instead
#' @param max_sampling_size
#' @param epoch_name
#'
#' @return a named vector of 3 numeric: interval start position, end position, length
#' @export
#'
#' @examples
FindSamplingInterval <- function(abf, current_chan_id = 0, voltage_chan_id = 0,
                                 min_sampling_size = 0, max_sampling_size = 0,
                                 allowed_voltage_delta = 0, epoch_name = "auto",
                                 backward_search = TRUE, strict_comp = TRUE) {

  #figure out current channel and voltage channel
  if (current_chan_id == 0)
    current_chan_id <- GetFirstCurrentChan(abf)
  if (voltage_chan_id == 0)
    voltage_chan_id <- GetFirstVoltageChan(abf)
  if (is.na(current_chan_id))
    stop("FindSamplingInterval: Failed to identify current channel id. Please provide manually.")
  if (is.na(voltage_chan_id))
    stop("FindSamplingInterval: Failed to identify voltage channel id. Please provide manually.")

  meta <- attr(abf, "meta")
  if (epoch_name == "auto") {
    n_epoch <- nrow(meta$EpochPerDAC)
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
    allowed_voltage_delta = abs(meta$EpochPerDAC$fEpochLevelInc[epoch] * 0.05)
  }

  #TODO: exclude intervals that are in irrelevant epochs
  #TODO: parse EpochPerDAC
  pts_per_epi <- dim(abf)[2]
  epoch_len <- meta$EpochPerDAC$lEpochInitDuration
  delta_pts <- pts_per_epi - sum(epoch_len)
  #pad extra pts to 1st epoch
  epoch_len[1] <- epoch_len[1] + delta_pts
  epoch_end <- cumsum(epoch_len)
  epoch_start <- epoch_end - epoch_len + 1
  epoch_idx <- cbind(epoch_start, epoch_end, epoch_len)

  #Default minimal sampling size is 10ms/10000us scan
  if (min_sampling_size == 0) {
    min_sampling_size <- floor(10000.0 / attr(abf, "SamplingInterval"))
  }
  #Force min sampling size to 3, so that sd makes sense
  if (min_sampling_size < 3) {
    min_sampling_size <- 3
  }
  #Default max sampling size is 10x min sampling size of 1/10 of epoch length,
  #whichever is larger
  s1 <- min_sampling_size * 10
  s2 <- floor(epoch_idx[epoch, 3] / 10)
  if (max_sampling_size == 0)
    max_sampling_size = max(s1, s2)

  #Collect available episodes
  epi <- AvailEpisodes(abf)
  #Calculate corresponding target voltage for each episode
  v_init <- meta$EpochPerDAC$fEpochInitLevel[epoch]
  v_incr <- meta$EpochPerDAC$fEpochLevelInc[epoch]
  v <- v_init + v_incr * (epi - 1L)

  #Find voltage windows that fit into allowed voltages
  intv <- GetAllowedWindows(abf[voltage_chan_id, ,epi[1]], v[1], allowed_voltage_delta,
                            min_sampling_size)
  nepi <- length(epi)
  #in case only one episode is available
  if (nepi > 1) {
    for (i in 2:length(epi)) {
      win <- GetAllowedWindows(abf[voltage_chan_id, ,epi[i]], v[i], allowed_voltage_delta,
                               min_sampling_size)
      intv <- GetWindowsOverlap(intv, win)
    }
  }

  #constrain intv to current epoch
  intv <- GetWindowsOverlap(intv, epoch_idx[epoch, , drop = FALSE])
  #if intv is too large, split them by max_sampling_size
  intv <- SplitLargeWindow(intv, max_sampling_size)

  if (is.null(intv) || nrow(intv) == 0) {
    s <- sprintf("FindSamplingInterval: No stable voltage interval found. Returning NA.
    allowed_voltage_delta = %.3f %s
    min_sampling_size = %d pts",
                 allowed_voltage_delta, attr(abf, "ChannelUnit")[voltage_chan_id], min_sampling_size)
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
                   allowed_voltage_delta, attr(abf, "ChannelUnit")[voltage_chan_id], min_sampling_size)
      warning(s)
      return(rep(NA, 3))
    }
    warning("FindSamplingInterval: Stable voltage intervals are smaller than min_sampling_size.")
  }
  #If failed to find voltage intervals larger than min_sampling_size, proceed
  #as is with those smaller intervals and throw a warning.
  intv <- tmp

  #function for comparing scores.
  f <- ifelse(strict_comp, score_all, score_mostly)
  best_score <- rep(Inf, nepi)
  best_idx <- 0
  if (backward_search)
    itr <- seq(nrow(intv), 1)
  else
    itr <- seq_len(nrow(intv))
  for (i in itr) {
    mask <- seq(intv[i, 1], intv[i, 2])
    #in case only one episode is available
    if (nepi > 1) {
      score <- colSds(abf[current_chan_id, mask, epi], na.rm = TRUE)
    } else {
      score <- sd(abf[current_chan_id, mask, epi], na.rm = TRUE)
    }
    if (f(score, best_score)) {
      best_score <- score
      best_idx <- i
    }
  }

  return(intv[best_idx, ])
}

#' Title
#'
#' @param abf_list
#' @param current_chan_id
#' @param voltage_chan_id
#' @param min_sampling_size
#' @param max_sampling_size
#' @param allowed_voltage_delta
#' @param epoch_name
#' @param backward_search
#' @param strict_comp
#'
#' @return
#' @export
#'
#' @examples
FindAllSamplingInterval <- function(abf_list, current_chan_id = 0, voltage_chan_id = 0,
                                    min_sampling_size = 0, max_sampling_size = 0,
                                    allowed_voltage_delta = 0, epoch_name = "auto",
                                    backward_search = TRUE, strict_comp = TRUE) {

  intv_list = list()
  for (i in seq_along(abf_list)) {
    intv_list[[i]] <- FindSamplingInterval(abf_list[[i]], current_chan_id, voltage_chan_id,
                                           min_sampling_size, max_sampling_size,
                                           allowed_voltage_delta, epoch_name,
                                           backward_search, strict_comp)
  }

  return(intv_list)
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
    stop("AllSamples_IVSummary: Failed to identify current channel id. Please provide manually.")
  if (is.na(voltage_chan_id))
    stop("AllSamples_IVSummary: Failed to identify voltage channel id. Please provide manually.")

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
GetFirstVoltageChan <- function(abf) match("Voltage", attr(abf,"ChannelDesc"))
GetFirstCurrentChan <- function(abf) match("Current", attr(abf,"ChannelDesc"))
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
FilterMinWindowPos <- function(windows, min_window_pos) {

  mask <- windows[, 2] >= min_window_pos

  return(windows[mask, , drop = FALSE])
}
FilterMaxWindowPos <- function(windows, max_window_pos) {

  mask <- windows[, 1] <= max_window_pos

  return(windows[mask, , drop = FALSE])
}
SplitLargeWindow <- function(windows, max_window_size) {

  idx <- 0
  idx_start <- c()
  idx_end <- c()
  win_length <- c()

  for (i in seq_len(nrow(windows))) {
    if (windows[i, 3] <= max_window_size) {
      idx <- idx + 1
      idx_start[idx] <- windows[i, 1]
      idx_end[idx] <- windows[i, 2]
      win_length[idx] <- windows[i, 3]
    } else {
      #window too large, split it
      new_start <- seq(from = windows[i, 1], to = windows[i, 2], by = max_window_size)
      for (j in 1:(length(new_start) - 1L)) {
        idx <- idx + 1
        idx_start[idx] <- new_start[j]
        idx_end[idx] <- new_start[j + 1] - 1
        win_length[idx] <- max_window_size
      }
      #last piece
      idx <- idx + 1
      idx_start[idx] <- new_start[length(new_start)]
      idx_end[idx] <- windows[i, 2]
      win_length[idx] <- idx_end[idx] - idx_start[idx] + 1
    }
  }

  win <- cbind(idx_start, idx_end, win_length)
  return(win)
}
ChannelInterval_f <- function(abf_list, intv_list, chan_id, f) {

  n <- length(abf_list)
  nepi <- EpisodesPerChannel(abf_list[[1]])

  m <- matrix(NA, nrow = n, ncol = nepi)
  colnames(m) <- paste0("epi", seq_len(nepi))

  for (i in seq_along(abf_list)) {
    if (any(is.na(intv_list[[i]]))) {
      next
    } else {
      mask <- seq(intv_list[[i]][1], intv_list[[i]][2])
      ret <- f(abf_list[[i]][chan_id, mask, ])
      for (j in seq(nepi))
        m[i, j] <- ret[j]
    }
  }

  return(m)
}

#Is this possible to optimise?
within_interval <- function(x, intv) intv[1] <= x && x <= intv[2]

score_all <- function(s1, best_s) {

  return(all(s1 < best_s))
}

score_mostly <- function(s1, best_s) {

  x <- (s1 < best_s)
  n <- sum(as.logical(x))
  #Criteria 1: most of s1 is better than best_s
  criteria1 <- (n * 2L  - 1L) > length(x)

  idx_max <- which.max(s1)
  #Criteria 2: the most deviated element of s1 is improved
  criteria2 <- s1[idx_max] < best_s[idx_max]

  return(criteria1 && criteria2)
}
