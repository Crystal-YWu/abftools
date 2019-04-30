GetEpdac <- function(abf, dac) {

  meta <- get_meta(abf)
  epdac <- meta$EpochPerDAC

  mask <- epdac$nDACNum %in% (dac - 1L)
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

  dimnames(win) <- list(c("startPos", "endPos", "length"), GetEpiTag(abf), NULL)
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
