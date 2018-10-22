#' Convert epoch name to epoch id
#'
#' @param epoch_name name of the epoch, A-J
#'
#' @return id of the epoch, epoch id is 1-based.
#' @export
#'
GetEpochId <- function(epoch_name) {

  epoch <- 0
  epoch_names <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
  epoch <- match(epoch_name, epoch_names)

  return(epoch)
}

#' Get DAC id of which waveform is enabled.
#'
#' @param abf an abf object.
#'
#' @return DAC id, 1-based.
#' @export
#'
GetWaveformEnabledDAC <- function(abf) {

  meta <- get_meta(abf)

  ret <- which(as.logical(meta$DAC$nWaveformEnable))
  if (length(ret) > 1L) {
    #not sure if this could really happen in real life
    warning("WaveformDAC: Multiple waveform DAC enabled.")
  }

  return(ret)
}

#I don't think we need to export this
GetWaveformEpdac <- function(abf, wf_dac) {

  meta <- get_meta(abf)
  epdac <- meta$EpochPerDAC
  #nDACNum is 0-based
  mask <- epdac$nDACNum == (wf_dac - 1L)
  #sort epdac by nEpochNum just in case
  ret <- epdac[mask, ]
  ret <- ret[order(ret$nEpochNum), ]

  return(ret)
}

#' Get intervals of all epochs.
#'
#' The returned array contains all interval info, which can be accessed by:
#' epoch[ , epoch_id, episode], an interval is defined as c(intv_start, intv_end, intv_length)
#'
#' @param abf an abf object.
#' @param wf_dac id of the waveform DAC, 1-based.
#'
#' @return a 3-d array, see details.
#' @export
#'
GetEpochIntervals <- function(abf, wf_dac = 0) {

  if (wf_dac[1] == 0) {
    wf_dac <- GetWaveformEnabledDAC(abf)
  }
  if (length(wf_dac) == 0L) {
    err_wf_dac("GetEpochIntervals")
  }
  wf_dac <- FirstElement(wf_dac)

  #EpochPerDAC table
  epdac <- GetWaveformEpdac(abf, wf_dac)

  #length of first holding
  npts <- nPts(abf)
  holding_len <- npts %/% 64L

  #length of each epoch
  init_len <- epdac$lEpochInitDuration
  incr_len <- epdac$lEpochDurationInc

  #pre-allocate win
  nepi <- nEpi(abf)
  nepoch <- nrow(epdac)
  win <- array(0L, dim = c(3L, nepoch, nepi))

  for (epi in seq.int(nepi)) {

    epoch_len <- init_len + incr_len * (epi - 1L)
    #shift epoch end idx accroding to first holding length
    epoch_end <- cumsum(epoch_len) + holding_len
    epoch_start <- epoch_end - epoch_len + 1L

    win[1L, , epi] <- epoch_start
    win[2L, , epi] <- epoch_end
    win[3L, , epi] <- epoch_len

  }

  return(win)
}
