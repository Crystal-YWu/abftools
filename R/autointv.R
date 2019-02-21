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
  epoch_intv <- GetEpochIntervals(abf)[, epoch, ]

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
#' 2. The function is looking for most stable voltage AND current channel.
#' 3. The function prefers stability to the size of interval.
#' 4. Intervals closer to the end of the epoch are preferred.
#'
#' @param abf an abf object.
#' @param current_channel OPTIONAL, channel id for current data, channel id is 1-based.
#' @param voltage_channel OPTIONAL, channel id for voltage data, channel id is 1-based.
#' @param target_interval_size OPTIONAL, target size in **points** of the sampling interval. Default is 20ms of samples.
#' @param allowed_voltage_delta OPTIONAL, allowed max deviation of voltage.
#' @param epoch OPTIONAL, the epoch to search, defaults to B (second epoch).
#' @param backward_search OPTIONAL, perform search along backward direction.
#' @param noisy_data OPTIONAL, set to TURE if data is noisy, may improve position of the predicted sampling interval.
#'
#' @return a named vector of 3 numeric: interval start position, end position, length
#' @export
#'
FindSamplingInterval <- function(abf, epoch = "B",
                                 current_channel = GetFirstCurrentChan(abf),
                                 voltage_channel = GetFirstVoltageChan(abf),
                                 target_interval_size = NULL,
                                 allowed_voltage_delta = NULL,
                                 backward_search = TRUE, noisy_data = FALSE) {

  if (IsAbfList(abf)) {
    return(
      lapply(abf, FindSamplingInterval, epoch = epoch,
             current_channel = current_channel,
             voltage_channel = voltage_channel,
             target_interval_size = target_interval_size,
             allowed_voltage_delta = allowed_voltage_delta)
    )
  }

  if (is.character(epoch)) {
    epoch <- GetEpochId(epoch)
  }
  CheckArgs(abf, chan = c(current_channel, voltage_channel), epo = epoch)

  if (is.null(allowed_voltage_delta)) {
    epdac <- GetEpdac(abf, FirstElement(GetWaveformEnabledDAC(abf)))
    v_settings <- epdac$fEpochInitLevel[epoch] +
                  epdac$fEpochLevelInc[epoch] * (seq_len(nEpi(abf)) - 1L)
    allowed_voltage_delta <- min(max(abs(v_settings)) * 0.10,
                                 abs(epdac$fEpochLevelInc[epoch]))
  }

  #Default target sampling size is 20ms/20000us scan
  if (is.null(target_interval_size)) {
    target_interval_size <- floor(20000.0 / GetSamplingIntv(abf))
  }
  target_interval_size <- max(3L, target_interval_size)

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
  ovlp <- OverlapEpisodicIntv(episodic_intv, npts)
  ovlp <- FilterMinIntervalSize(ovlp, target_interval_size)
  if (!ncol(ovlp)) {
    fmt_str <- "In %s, no common stable voltage interval found for all episodes. "
    warning(sprintf(fmt_str, GetTitle(abf)))
    return(rep(NA, 3))
  }

  #CURRENT
  sr <- max(3L, target_interval_size %/% 10L)
  channel_data <- abf[[current_channel]]
  channel_var <- rowSums(samplend(channel_data,
                                  sampling_ratio = sr,
                                  colFunc = matrixStats::colSds))
  channel_var <- rep(channel_var, times = sr, length.out = npts)

  #CURRENT
  if (backward_search) {
    itr <- seq(ncol(ovlp), 1L)
  } else {
    itr <- seq_len(ncol(ovlp))
  }
  best_score <- Inf
  best_intv <- c(0, 0, 0)

  for (i in itr) {
    search_result <- BinSearchIntv(channel_var, ovlp[ , i], target_interval_size)
    if (search_result$score < best_score) {
      best_score <- search_result$score
      best_intv <- search_result$intv
    }
  }

  best_intv
}

#' @rdname FindSamplingInterval
#' @export
#'
FindAllSamplingInterval <- function(abf_list, ...) {

  CheckArgs(abf_list, allow_list = TRUE)
  lapply(abf_list, FindSamplingInterval, ...)
}

#' Print a quick waveform channel summary of a sampling interval.
#'
#' @param abf an abf object.
#' @param intv an interval to summarise.
#' @param channel OPTIONAL, the channel to compare.
#' @param epoch OPTIONAL, the epoch to compare.
#'
#' @return Nothing
#' @export
#'
CheckSamplingInterval <- function(abf, intv, channel, epoch = "B") {

  if (!IsAbf(abf)) {
    err_class_abf()
  }


  if (missing(channel) || is.null(channel)) {
    channel <- GetFirstVoltageChan(abf)
  } else if (!AssertChannel(abf, channel)) {
    err_channel()
  }
  if (is.character(epoch[1])) {
    epoch <- GetEpochId(epoch)
  }
  epoch <- FirstElement(epoch)
  if (!AssertEpoch(abf, epoch)) {
    err_epoch()
  }

  episodes <- GetAvailEpisodes(abf)
  wf <- GetWaveform(abf, episodes)
  wf_chan <- FirstElement(GetWaveformEnabledDAC(abf))
  meta <- get_meta(abf)
  wf_unit <- meta$Strings[meta$DAC$lDACChannelUnitsIndex[wf_chan]]
  settings <- meta$EpochPerDAC$fEpochInitLevel[epoch] +
    (seq_len(nEpi(abf)) - 1) * meta$EpochPerDAC$fEpochLevelInc[epoch]

  for (i in seq_along(episodes)) {

    mask <- MaskIntv(intv)

    delta <- abs(abf[mask, episodes[i], channel] - wf[mask, episodes[i]])
    max_abs <- max(delta)
    max_rel <- max_abs / settings[episodes[i]]
    means <- mean(abf[mask, episodes[i], channel])
    sems <- stats::sd(abf[mask, episodes[i], channel]) /
      sqrt(length(abf[mask, episodes[i], channel]))

    s <- sprintf("Episode %d: Max absolute delta %.2f %s (relative %.2f, mean %.2f, SEM %.2f)\n",
                 episodes[i], max_abs, wf_unit, max_rel, means, sems)
    cat(s)
  }

}
