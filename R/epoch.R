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
