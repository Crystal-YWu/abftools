allowed_delta_abs <- function(channel_data, delta) {

  d <- dim(channel_data)
  ret <- array(data = abs(delta), dim = d)

  return(ret)
}

allowed_delta_rel <- function(channel_data, delta) {

  ret <- abs(channel_data * delta)

  return(ret)
}

#' Title
#'
#' @param abf
#' @param channel
#' @param epoch
#' @param delta
#' @param relative
#'
#' @return
#' @export
#'
#' @examples
CmpWaveform <- function(abf, channel, epoch, delta, relative,
                        min_win = 0 , max_win = 0) {

  epoch_intv <- GetEpochIntervals(abf)
  epoch <- first_elem(epoch)

  episodes <- GetAvailEpisodes(abf)
  wf <- GetWaveform(abf, episodes)
  if (relative) {
    wf_allowed <- allowed_delta_rel(wf, delta)
  } else {
    wf_allowed <- allowed_delta_abs(wf, delta)
  }
  wf_delta <- abs(wf - abf[[channel]])

  ret <- list()
  for (i in seq.int(episodes)) {

    intv <- epoch_intv[, epoch, episodes[i]]

    mask <- MaskIntv(intv)
    v <- wf_delta[mask, i] <= wf_allowed[mask, i]

    tmp <- LogiToIntv(v)

    if (nrow(tmp) > 0L) {
      #shift according to position of win
      tmp[1, ] <- tmp[1, ] + intv[1] - 1L
      tmp[2, ] <- tmp[2, ] + intv[1] - 1L
      #filter length
      if (min_win != 0L) {
        tmp_idx <- min_win <= tmp[3, ]
        tmp <- tmp[, tmp_idx, drop = FALSE]
      }
      if (max_win != 0L) {
        tmp_idx <- tmp[3, ] <= max_win
        tmp <- tmp[, tmp_idx, drop = FALSE]
      }
    }

    ret[[episodes[i]]] <- tmp
  }

  return(ret)
}

#' FindSamplingInterval finds a stable interval for sampling current and voltage data of an abf object
#'
#' @param abf an abf object
#' @param current_channel OPTIONAL, channel id for current data
#' @param voltage_channel OPTIONAL, channel id for voltage data
#' @param min_sampling_size OPTIONAL, min size in points of a sampling interval
#' @param allowed_voltage_delta OPTIONAL, allowed max deviation of voltage W.R.T epoch per DAC setting
#' @param epoch_name OPTIONAL, the epoch to search, defaults to B (second epoch)
#' @param backward_search OPTIONAL, perform search along backward direction
#'
#' @return a named vector of 3 numeric: interval start position, end position, length
#' @export
#'
#' @examples
FindSamplingInterval <- function(abf, current_channel = 0, voltage_channel = 0,
                                 min_sampling_size = 0, allowed_voltage_delta = 0,
                                 epoch_name = "B", backward_search = TRUE) {

  #figure out current channel and voltage channel
  if (current_channel == 0) {
    current_channel <- GetFirstCurrentChan(abf)
  }
  if (voltage_channel == 0) {
    voltage_channel <- GetFirstVoltageChan(abf)
  }
  if (is.na(current_channel)) {
    err_id_current_chan("FindSamplingInterval")
  }
  if (is.na(voltage_channel)) {
    err_id_voltage_chan("FindSamplingInterval")
  }

  epoch <- GetEpochId(epoch_name)

  #Default allowed voltage delta is 5% of voltage epoch level increment
  meta <- get_meta(abf)
  if (allowed_voltage_delta == 0) {
    allowed_voltage_delta = abs(meta$EpochPerDAC$fEpochLevelInc[epoch] * 0.05)
  }

  #Default minimal sampling size is 10ms/10000us scan
  if (min_sampling_size == 0) {
    min_sampling_size <- floor(10000.0 / GetSamplingIntv(abf))
  }
  #Force min sampling size to 3, so that sd makes sense
  if (min_sampling_size < 3L) {
    min_sampling_size <- 3L
  }

  #calculate episodic intervals
  episodic_intv <- CmpWaveform(abf, channel = voltage_channel,  epoch = epoch,
                          delta = allowed_voltage_delta, relative = FALSE,
                          min_win = min_sampling_size)

  episodes <- GetAvailEpisodes(abf)
  flag <- FALSE
  flagged <- c()
  for (epi in episodes) {
    if (ncol(episodic_intv[[epi]]) == 0L) {
      flag <- TRUE
      flagged <- c(epi, flagged)
    }
  }
  if (flag) {
    fmt_str <- paste0("In %s, no stable voltage interval found for episode %s. ",
                      "Returning NA.")
    s <- sprintf(fmt_str, GetTitle(abf), paste0(sort(flagged), collapse = ", "))
    warning(s)
    return(rep(NA, 3))
  }

  npts <- nPts(abf)
  ovlp <- OverlapEpisodicIntv(episodic_intv, episodes, npts)
  mask <- min_sampling_size <= ovlp[3, ]
  ovlp <- ovlp[ , mask, drop = FALSE]
  if (ncol(ovlp) == 0L) {
    fmt_str <- paste0("In %s, no common stable voltage interval found for all episodes. ",
                      "Returning NA.")
    s <- sprintf(fmt_str, GetTitle(abf))
    warning(s)
    return(rep(NA, 3))
  }

  if (backward_search) {
    itr <- seq.int(ncol(ovlp), 1L)
  } else {
    itr <- seq.int(ncol(ovlp))
  }
  best_score <- rep(Inf, length(episodes))
  best_intv <- c(0, 0, 0)
  channel_data <- abf[[current_channel]]
  for (epi in itr) {
    search_result <- BinSearchIntv(channel_data, ovlp[ , epi], min_sampling_size, score_worst_half)
    if (score_worst_half(search_result$score, best_score)) {
      best_score <- search_result$score
      best_intv <- search_result$intv
    }
  }

  return(best_intv)
}

OverlapEpisodicIntv <- function(episodic_intv, episodes, npts) {

  ret <- rep(TRUE, npts)
  for (epi in episodes) {
    ret <- ret & IntvToLogi(episodic_intv[[epi]], npts)
  }

  return(LogiToIntv(ret))
}

BinSearchIntv <- function(channel_data, intv, min_intv, f) {

  mask <- MaskIntv(intv)
  ret <- list()
  ret$score <- colSds(channel_data[mask, ])
  ret$intv <- intv

  if (intv[3] %/% 2L <= min_intv) {
    return(ret)
  }

  mid_idx <- (intv[1] + intv[2]) %/% 2L
  intv_l <- c(intv[1], mid_idx, mid_idx - intv[1] + 1L)
  intv_r <- c(mid_idx, intv[2], intv[2] - mid_idx + 1L)

  ret_r <- BinSearchIntv(channel_data, intv_r, min_intv , f)
  if (f(ret_r$score, ret$score)) {
    return(ret_r)
  } else {
    ret_l <- BinSearchIntv(channel_data, intv_l, min_intv, f)
    if (f(ret_l$score, ret$score)) {
      return(ret_l)
    } else {
      return(ret)
    }
  }
}

score_worst_half <- function(s1, best_s) {

  #half length + 1
  n <- length(best_s) %/% 2L + 1L
  o <- order(best_s, decreasing = TRUE)
  mask <- o[seq.int(n)]

  return(all(s1[mask] < best_s[mask]))
}
