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

  idx <- as.logical(meta$DAC$nWaveformEnable)
  nDACNum <- meta$DAC$nDACNum[idx]
  if (length(nDACNum) > 1L) {
    #TODO: parse all enabled DAC channel.
    warning("GetWaveformEnabledDAC: Multiple waveform DAC enabled.")
  }

  #force DACid <-> nDACNum conversion
  DACid <- nDACNum + 1L

  #We can't rely solely on DAC$nWaveformEnable because even if user selected other
  #modes which waveforms are not enabled and corresponding settings are grayed
  #out in Clampex software, somehow the program will write all previous Waveform
  #settings.
  #
  #Check if EpochPerDAC is present
  epdac <- meta$EpochPerDAC
  if (is.null(epdac)) {
    return(integer())
  }

  return(DACid)
}

#I don't think we need to export this
GetWaveformEpdac <- function(abf, DACid) {

  meta <- get_meta(abf)
  epdac <- meta$EpochPerDAC
  nDACNum <- DACid - 1L

  #nDACNum is 0-based
  mask <- epdac$nDACNum == nDACNum
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
#' @param wf_dac_id id of the waveform DAC, 1-based.
#'
#' @return a 3-d array, see details.
#' @export
#'
GetEpochIntervals <- function(abf, wf_dac_id = 0) {

  if (wf_dac_id[1] == 0) {
    wf_dac_id <- GetWaveformEnabledDAC(abf)
  }
  if (length(wf_dac_id) == 0L) {
    #The abf is not waveform stimulus mode, return epoch as whole episode
    nepi <- nEpi(abf)
    npts <- nPts(abf)
    ret <- array(c(1L, npts, npts), dim = c(3L, 1L, nepi))
    return(ret)
  }
  wf_dac_id <- FirstElement(wf_dac_id)

  #EpochPerDAC table
  epdac <- GetWaveformEpdac(abf, wf_dac_id)

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
