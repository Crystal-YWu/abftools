IsVoltageUnit <- function(x) endsWith(toupper(x), "V") | grepl("VO", toupper(x), fixed = TRUE)
IsCurrentUnit <- function(x) endsWith(toupper(x), "A") | grepl("AM", toupper(x), fixed = TRUE)

#' Get voltage/current channel id.
#'
#' If abf is a list of abf objects, returns common voltage/current channel id.
#'
#' @param abf an abf object or a list of abf objects.
#'
#' @return a vector of voltage/current channel id.
#' @export
#'
GetVoltageChan <- function(abf) {

  CheckArgs(abf, allow_list = TRUE)

  f <- function(x) which(IsVoltageUnit(GetChannelUnit(x)))
  if (IsAbf(abf)) {
    ans <- f(abf)
  } else {
    ans <- Reduce(intersect, lapply(abf, f))
    if (!length(ans)) {
      err_channel_config(abf)
    }
  }

  ans
}

#' @rdname GetVoltageChan
#' @export
#'
GetCurrentChan <- function(abf) {

  CheckArgs(abf, allow_list = TRUE)

  f <- function(x) which(IsCurrentUnit(GetChannelUnit(x)))
  if (IsAbf(abf)) {
    ans <- f(abf)
  } else {
    ans <- Reduce(intersect, lapply(abf, f))
    if (!length(ans)) {
      err_channel_config(abf)
    }
  }

  ans
}

#' Get first voltage/current channel id.
#'
#' @param abf an abf object or a list of abf objects.
#'
#' @return an integer id of first voltage/current channel.
#' @export
#'
GetFirstVoltageChan <- function(abf) {

  voltage_channel <- GetVoltageChan(abf)
  FirstElement(voltage_channel)
}

#' @rdname GetFirstVoltageChan
#' @export
#'
GetFirstCurrentChan <- function(abf) {

  current_channel <- GetCurrentChan(abf)
  FirstElement(current_channel)
}

#' Return all channels of an abf object.
#'
#' @param abf an abf object.
#'
#' @return a vector of channel ids.
#' @export
#'
GetAllChannels <- function(abf) {

  if (IsAbf(abf)) {
    seq_len(nChan(abf))
  } else if (IsAbfList(abf)) {
    lapply(abf, function(x) seq_len(nChan(x)))
  } else {
    err_class_abf()
  }
}
