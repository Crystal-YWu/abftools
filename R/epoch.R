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

#' Look for first epoch that can perform a membrane test.
#'
#' @param abf an abf object.
#' @param dac dac channel.
#' @param type type of membrane test. When type is "step", the function searches
#' for pattern of holding level -> step -> holding level. When type is "ramp",
#' the function searches for pattern of holding level -> ramp up/down -> ramp down/up
#' -> holding level.
#'
#' @return an epoch number
#' @export
#'
FindMemtestEpoch <- function(abf, dac = GetWaveformEnabledDAC(abf), type = c("step", "ramp")) {

  CheckArgs(abf, dac = dac)
  dac <- FirstElement(dac)
  type <- match.arg(type)

  switch(type,
         step = FindMemtestEpoch_Step(abf, dac),
         ramp = FindMemtestEpoch_Ramp(abf, dac),
         integer(0))
}

FindMemtestEpoch_Step <- function(abf, dac) {

  # What we find: holding lvl + delta step (+ holding lvl)
  #
  #        |------|
  #        |      |
  #  ______|      |______
  #

  nepo <- nEpoch(abf, dac = dac)
  epdac <- GetEpdac(abf, dac = dac)
  for (epo in seq_len(nepo - 1L)) {
    if (epdac$nEpochType[epo] == 1 &&  epdac$fEpochLevelInc[epo] == 0 && epdac$nEpochType[epo + 1L] == 1 &&
        epdac$fEpochInitLevel[epo + 1L] != epdac$fEpochInitLevel[epo] || epdac$fEpochLevelInc[epo + 1L] != 0) {
      return(epo + 1L)
    }
  }

  integer(0)
}

FindMemtestEpoch_Ramp <- function(abf, dac) {

  # What we find: (holding lvl +) V-shaped ramp (+ holding lvl)
  #
  # -----\         /-----
  #       \      /
  #        \   /
  #         \/

  nepo <- nEpoch(abf, dac = dac)
  epdac <- GetEpdac(abf, dac = dac)

  for (epo in seq_len(nepo - 1L)) {
    #assume starting from holding
    if (epdac$nEpochType[epo] == 2 && epdac$nEpochType[epo + 1L] == 2) {
      return(epo)
    }
  }

  integer(0)
}
