#' Convert epoch name to epoch id
#'
#' @param epoch_name name of the epoch, A-J
#'
#' @return id of the epoch, epoch id is 1-based.
#' @export
#'
GetEpochId <- function(epoch_name) {

  epoch_names <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
  epoch <- match(epoch_name, toupper(epoch_names))

  if (any(is.na(epoch))) {
    err_epoch_name()
  }

  epoch
}

#' Get DAC id of which waveform is enabled.
#'
#' @param abf an abf object.
#'
#' @return DAC id, 1-based.
#' @export
#'
GetWaveformEnabledDAC <- function(abf) {

  CheckArgs(abf)

  meta <- get_meta(abf)

  mask <- as.logical(meta$DAC$nWaveformEnable)
  meta$DAC$nDACNum[mask] + 1L
}

GetEpdac <- function(abf, dac) {

  meta <- get_meta(abf)
  epdac <- meta$EpochPerDAC

  mask <- epdac$nDACNum == (dac - 1L)
  epdac[mask, ]
}

#' Get intervals of all epochs.
#'
#' The returned array contains all interval info, which can be accessed by:
#' epoch[ , episode, epoch_id], an interval is defined as c(intv_start, intv_end, intv_length)
#'
#' @param abf an abf object.
#' @param dac OPTIONAL, DAC channel, 1-based.
#'
#' @return a 3-d array, see details.
#' @export
#'
GetEpochIntervals <- function(abf, dac = GetWaveformEnabledDAC(abf)) {

  CheckArgs(abf, dac = dac)

  epdac <- GetEpdac(abf, FirstElement(dac))

  #length of first holding
  npts <- nPts(abf)
  holding_len <- npts %/% 64L

  #length of each epoch
  init_len <- epdac$lEpochInitDuration
  incr_len <- epdac$lEpochDurationInc

  #pre-allocate win
  nepi <- nEpi(abf)
  nepoch <- nrow(epdac)
  win <- array(0L, dim = c(3L, nepi, nepoch))

  for (epi in seq_len(nepi)) {

    epoch_len <- init_len + incr_len * (epi - 1L)
    #shift epoch end idx accroding to first holding length
    epoch_end <- cumsum(epoch_len) + holding_len
    epoch_start <- epoch_end - epoch_len + 1L

    win[1L, epi, ] <- epoch_start
    win[2L, epi, ] <- epoch_end
    win[3L, epi, ] <- epoch_len

  }

  dimnames(win) <- list(c("startPos", "endPos", "length"), NULL, NULL)

  win
}

#' Return all step epoch id.
#'
#' @param abf an abf object.
#' @param dac DAC channel.
#'
#' @return an integer vector.
#' @export
#'
GetStepEpoch <- function(abf, dac = GetWaveformEnabledDAC(abf)) {

  CheckArgs(abf, dac = dac)

  epdac <- GetEpdac(abf, FirstElement(dac))
  which(epdac$nEpochType == 1)
}

#' @rdname GetStepEpoch
#' @export
#'
GetMultiStepEpoch <- function(abf, dac = GetWaveformEnabledDAC(abf)) {

  CheckArgs(abf, dac = dac)

  epdac <- GetEpdac(abf, FirstElement(dac))
  which(epdac$nEpochType == 1 & epdac$fEpochLevelInc != 0)
}
