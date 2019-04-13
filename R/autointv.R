#' Compare epoch waveform setting to a channel data.
#'
#' @param abf an abf object.
#' @param episode the episodes to compare.
#' @param channel the channel to compare, channel id is 1-based.
#' @param epoch the epoch to compare.
#' @param delta allowed max deviation.
#' @param min_win OPTIONAL, minimum interval window size of the result.
#' @param max_win OPTIONAL, maximum interval window size of the result.
#'
#' @return a list of intervals of which pass comparison.
#' @export
#'
CmpWaveform <- function(abf, episode = GetAllEpisodes(abf), channel, epoch,
                        delta = NULL, min_win = NULL, max_win = NULL) {

  epoch <- FirstElement(epoch)
  if (is.character(epoch)) {
    epoch <- GetEpochId(epoch)
  }
  CheckArgs(abf, epi = episode, chan = channel, epo = epoch)

  nepi <- nEpi(abf)
  epoch_intv <- GetEpochIntervals(abf)[, , epoch]

  wf <- GetWaveform(abf)
  wf_delta <- abs(wf - abf[,, channel])
  if (is.null(delta)) {
    if (nepi == 1L) {
      mask <- MaskIntv(epoch_intv)
      delta <- stats::median(wf_delta)
    } else {
      delta <- sapply(seq_len(nepi), function(epi) {
        mask <- MaskIntv(epoch_intv[, epi])
        stats::median(wf_delta[mask, epi])
      })
    }
  }
  wf_cmp <- wf_delta <= allowed_delta_abs(wf, unlist(delta))

  if (nepi == 1L) {
    dim(epoch_intv) <- c(3L, 1L)
    dim(wf_cmp) <- c(length(wf_cmp), 1L)
  }

  ret <- list()
  for (epi in episode) {

    intv <- epoch_intv[, epi]
    mask <- MaskIntv(intv)

    tmp <- LogiToIntv(wf_cmp[mask, epi])
    if (ncol(tmp)) {
      #shift according to position of win
      tmp[1, ] <- tmp[1, ] + intv[1] - 1L
      tmp[2, ] <- tmp[2, ] + intv[1] - 1L
      #filter length
      if (!is.null(min_win)) {
        tmp <- FilterMinIntervalSize(tmp, min_win)
      }
      if (!is.null(max_win)) {
        tmp <- FilterMaxIntervalSize(tmp, max_win)
      }
    }

    ret[[epi]] <- tmp
  }

  ret
}

#' FindSamplingInterval finds a stable interval for sampling current and voltage data of an abf object.
#'
#' FindSamplingInterval is a convenient alternative to CmpWaveform, best suited
#' to locate sampling intervals when using step voltage cmd waveform. Some common
#' assumptions are made:
#' 1. Waveform Cmd is outputing voltage command.
#' 2. Waveform epochs are aligned, i.e. epoch duration incr is 0.
#' 3. The function is looking for most stable voltage AND current channel.
#' 4. The function prefers stability to the size of interval.
#' 5. Intervals closer to the end of the epoch are preferred.
#'
#' @param abf an abf object.
#' @param epoch the epoch to search, defaults to B (second epoch).
#' @param current_channel channel id for current data, channel id is 1-based.
#' @param voltage_channel channel id for voltage data, channel id is 1-based.
#' @param target_interval_size OPTIONAL, target size in **points** of the sampling interval.
#' Default is 2x lowpass window.
#' @param allowed_voltage_delta OPTIONAL, allowed max deviation of voltage.
#' @param noisy_opt enable optimisation for noisy data by applying a lowpass filter to current channel.
#' @param lp_freq frequency of low-pass filter.
#' @param lp_order order of low-pass filter.
#'
#' @return a named vector of 3 numeric: interval start position, end position, length
#' @export
#'
FindSamplingInterval <- function(abf, epoch = "B",
                                 current_channel = GetFirstCurrentChan(abf),
                                 voltage_channel = GetFirstVoltageChan(abf),
                                 target_interval_size = NULL,
                                 allowed_voltage_delta = NULL,
                                 noisy_opt = FALSE, lp_freq = 75, lp_order = 1L) {

  list_mode <- IsAbfList(abf)
  if (list_mode) {
    nabf <- length(abf)
    abf <- AverageAbf(abf)
  }

  if (is.character(epoch)) {
    epoch <- GetEpochId(epoch)
  }
  CheckArgs(abf, chan = c(current_channel, voltage_channel), epo = epoch)

  if (noisy_opt) {
    abf <- ApplyLowpass(abf, chan = current_channel, freq = lp_freq, order = lp_order)
  }

  epdac <- GetEpdac(abf, FirstElement(GetWaveformEnabledDAC(abf)))
  if (any(epdac$lEpochDurationInc != 0)) {
    err_epoch_align()
  }
  if (is.null(allowed_voltage_delta)) {
    v_settings <- epdac$fEpochInitLevel[epoch] +
                  epdac$fEpochLevelInc[epoch] * (seq_len(nEpi(abf)) - 1L)
    allowed_voltage_delta <- min(max(abs(v_settings)) * 0.10,
                                 abs(epdac$fEpochLevelInc[epoch]))
  }

  lp_interval_size <- FreqToTick(abf, freq = lp_freq)
  target_interval_size <- max(3L, ifelse(is.null(target_interval_size),
                                                 2L * lp_interval_size,
                                                 target_interval_size))

  #VOLTAGE
  episodes <- GetAvailEpisodes(abf)
  episodic_intv <- CmpWaveform(abf, channel = voltage_channel, epoch = epoch,
                               delta = allowed_voltage_delta, min_win = target_interval_size)

  flagged <- unlist(lapply(episodes, function(i) if (!ncol(episodic_intv[[i]])) i))
  if (!is.null(flagged)) {
    #Fallback
    episodic_fallback <- CmpWaveform(abf, episode = flagged, channel = voltage_channel,
                                     epoch = epoch, delta = NULL, min_win = target_interval_size)
    for (epi in flagged) {
      episodic_intv[[epi]] <- episodic_fallback[[epi]]
    }
  }
  flagged <- unlist(lapply(episodes, function(i) if (!ncol(episodic_intv[[i]])) i))
  if (!is.null(flagged)) {
    fmt_str <- "In %s, no stable voltage interval found for episode %s. "
    warning(sprintf(fmt_str, GetTitle(abf), paste0(flagged, collapse = ", ")))
    return(rep(NA, 3))
  }

  npts <- nPts(abf)
  ovlp <- FilterMinIntervalSize(OverlapEpisodicIntv(episodic_intv, npts),
                                target_interval_size)
  if (!ncol(ovlp)) {
    fmt_str <- "In %s, no common stable voltage interval found for all episodes. "
    warning(sprintf(fmt_str, GetTitle(abf)))
    return(rep(NA, 3))
  }

  #CURRENT
  sr <- max(3L, lp_interval_size %/% 10L)
  channel_var <- rowSums(samplend(abf[[current_channel]],
                                  ratio = sr,
                                  colFunc = matrixStats::colSds))
  channel_var <- td_penalty(rep(channel_var, each = sr, length.out = npts),
                            MaskIntv(GetEpochIntervals(abf)[, 1L, epoch]))

  #INTERVAL
  best_score <- Inf
  best_intv <- c(0, 0, 0)
  for (i in seq_len(ncol(ovlp))) {
    search_result <- BinSearchIntv(channel_var, ovlp[, i], target_interval_size)
    if (search_result$score < best_score) {
      best_score <- search_result$score
      best_intv <- search_result$intv
    }
  }

  if (list_mode) {
    rep(list(best_intv), nabf)
  } else {
    best_intv
  }
}

#' @rdname FindSamplingInterval
#' @export
#'
FindAllSamplingInterval <- function(abf_list, ...) {

  CheckArgs(abf_list, allow_list = TRUE)
  lapply(abf_list, FindSamplingInterval, ...)
}

