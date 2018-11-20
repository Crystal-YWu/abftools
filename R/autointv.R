#' Compare epoch waveform setting to a channel data.
#'
#' @param abf an abf object.
#' @param channel the channel to compare, channel id is 1-based.
#' @param epoch the epoch to compare.
#' @param delta allowed max deviation.
#' @param relative delta is given in relative value or absolute value.
#' @param min_win OPTIONAL, minimum interval window size of the result.
#' @param max_win OPTIONAL, maximum interval window size of the result.
#'
#' @return a list of intervals of which pass comparison.
#' @export
#'
CmpWaveform <- function(abf, channel, epoch, delta, relative = FALSE,
                        min_win = NULL, max_win = NULL) {

  if (!IsAbf(abf)) {
    err_class_abf()
  }
  if (!AssertChannel(abf, channel)) {
    err_channel()
  }
  epoch_intv <- GetEpochIntervals(abf)
  epoch <- FirstElement(epoch)
  if (is.character(epoch)) {
    epoch <- GetEpochId(epoch)
  }
  if (!AssertEpoch(abf, epoch)) {
    err_epoch()
  }
  if (relative && delta >= 1.0) {
    warning("Very large delta is given in relative mode. Please notice delta is not a percentage.")
  }

  episodes <- GetAvailEpisodes(abf)
  wf <- GetWaveform(abf, episodes)

  delta <- unlist(delta)
  if (relative) {
    wf_allowed <- allowed_delta_rel(wf, delta)
  } else {
    wf_allowed <- allowed_delta_abs(wf, delta)
  }
  wf_delta <- abs(wf - abf[[channel]])

  ret <- list()
  for (i in seq_along(episodes)) {

    intv <- epoch_intv[, epoch, episodes[i]]

    mask <- MaskIntv(intv)
    v <- wf_delta[mask, i] <= wf_allowed[mask, i]

    tmp <- LogiToIntv(v)

    if (nrow(tmp) > 0L) {
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

    ret[[episodes[i]]] <- tmp
  }

  return(ret)
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
#' @param min_sampling_size OPTIONAL, min size in points of a sampling interval.
#' @param max_sampling_size OPTIONAL, max size in points of a sampling interval.
#' @param allowed_voltage_delta OPTIONAL, allowed max deviation of voltage.
#' @param epoch OPTIONAL, the epoch to search, defaults to B (second epoch).
#' @param backward_search OPTIONAL, perform search along backward direction.
#' @param noisy_data OPTIONAL, set to TURE if data is noisy, may improve position of the predicted sampling interval.
#'
#' @return a named vector of 3 numeric: interval start position, end position, length
#' @export
#'
FindSamplingInterval <- function(abf, current_channel, voltage_channel,
                                 min_sampling_size, max_sampling_size,
                                 allowed_voltage_delta, epoch = "B",
                                 backward_search = TRUE, noisy_data = FALSE) {

  if (!IsAbf(abf)) {
    err_class_abf()
  }

  #figure out current channel and voltage channel
  if (missing(current_channel) || is.null(current_channel)) {
    current_channel <- GetFirstCurrentChan(abf)
  }
  if (missing(voltage_channel) || is.null(voltage_channel)) {
    voltage_channel <- GetFirstVoltageChan(abf)
  }
  if (is.na(current_channel)) {
    err_id_current_chan()
  }
  if (is.na(voltage_channel)) {
    err_id_voltage_chan()
  }

  if (is.character(epoch)) {
    epoch <- GetEpochId(epoch)
  }
  if (!AssertEpoch(abf, epoch)) {
    err_epoch()
  }

  #Default allowed voltage delta is 5% of relative error
  meta <- get_meta(abf)
  if (missing(allowed_voltage_delta) || is.null(allowed_voltage_delta)) {

    v_settings <- meta$EpochPerDAC$fEpochInitLevel[epoch] +
      (seq_len(nEpi(abf)) - 1L) * meta$EpochPerDAC$fEpochLevelInc[epoch]
    v_settings <- v_settings * 0.05
    #workaround for zero v settings: set delta to mean.
    idx_zero <- which(v_settings == 0)
    v_settings[idx_zero] = mean(v_settings)

    allowed_voltage_delta <- v_settings
  }

  #Default minimal sampling size is 10ms/10000us scan
  if (missing(min_sampling_size) || is.null(min_sampling_size)) {
    min_sampling_size <- floor(10000.0 / GetSamplingIntv(abf))
  }
  #Force min sampling size to 3, so that sd makes sense
  if (min_sampling_size < 3L) {
    min_sampling_size <- 3L
  }
  if (missing(max_sampling_size) || is.null(max_sampling_size)) {
    max_sampling_size <- Inf
  }
  if (max_sampling_size < 3L) {
    max_sampling_size <- 3L
  }

  #calculate episodic intervals
  episodic_intv <- CmpWaveform(abf, channel = voltage_channel, epoch = epoch,
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
  ovlp <- FilterMinIntervalSize(ovlp, min_sampling_size)
  if (ncol(ovlp) == 0L) {
    fmt_str <- paste0("In %s, no common stable voltage interval found for all episodes. ",
                      "Returning NA.")
    s <- sprintf(fmt_str, GetTitle(abf))
    warning(s)
    return(rep(NA, 3))
  }

  if (backward_search) {
    itr <- seq(ncol(ovlp), 1L)
  } else {
    itr <- seq_len(ncol(ovlp))
  }
  best_score <- rep(Inf, length(episodes))
  best_intv <- c(0, 0, 0)
  channel_data <- abf[[current_channel]]

  if (noisy_data) {
    ff <- score_worst_half
  } else {
    ff <- score_all
  }

  for (i in itr) {
    search_result <- BinSearchIntv(channel_data, ovlp[ , i], min_sampling_size, ff)
    if (ff(search_result$score, best_score)) {
      best_score <- search_result$score
      best_intv <- search_result$intv
    }
  }

  if (best_intv[3] > max_sampling_size) {
    best_intv <- Intv(endPos = best_intv[2], len = max_sampling_size)
  }

  return(best_intv)
}

#' Call FindSamplingInterval for a list of abf objects.
#'
#' @param abf_list a list of abf objects.
#' @param ... other arguments to pass to FindSamplingInterval
#'
#' @return a list of found intervals.
#' @export
#'
FindAllSamplingInterval <- function(abf_list, ...) {

  if (!IsAbfList(abf_list)) {
    err_class_abf_list()
  }

  intv_list = list()
  for (i in seq_along(abf_list)) {
    intv_list[[i]] <- FindSamplingInterval(abf_list[[i]], ...)
  }

  return(intv_list)
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
  wf_unit <- meta$Strings[[meta$DAC$lDACChannelUnitsIndex[wf_chan]]]
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
