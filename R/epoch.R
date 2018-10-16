#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
WaveformDAC <- function(abf) {

  meta <- get_meta(abf)

  ret <- which(meta$DAC$nWaveformEnable)
  if (length(ret) > 1) {
    #not sure if this could really happen in real life
    warning("WaveformDAC: Multiple waveform DAC enabled.")
  }

  return(ret)
}

#' Title
#'
#' @param abf
#' @param epi
#' @param dac
#'
#' @return
#' @export
#'
#' @examples
GetEpochIdx <- function(abf, epi, dac = 0) {

  #EpochPerDAC table
  meta <- get_meta(abf)
  epdac <- meta$EpochPerDAC

  #length of first holding
  npts <- nPts(abf)
  holding_len <- npts %/% 64L

  #length of each epoch
  init_len <- epdac$lEpochInitDuration
  incr_len <- epdac$lEpochDurationInc
  epoch_len <- init_len + incr_len * (epi - 1L)
  #shift epoch end idx accroding to first holding length
  epoch_end <- cumsum(epoch_len) + holding_len
  epoch_start <- epoch_end - epoch_len + 1

  ret <- cbind(epoch_start, epoch_end, epoch_len)
  return(ret)
}

