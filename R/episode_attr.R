#' Return all episodes of an abf object.
#'
#' @param abf an abf object.
#'
#' @return a vector of episode numbers.
#' @export
#'
GetAllEpisodes <- function(abf) {

  if (IsAbf(abf)) {
    seq_len(nEpi(abf))
  } else if (IsAbfList(abf)) {
    lapply(abf, function(x) seq_len(nEpi(x)))
  } else {
    err_class_abf()
  }
}

#' Get available episodes
#'
#' @param abf an abf or a list of abf object.
#'
#' @return episodes that are not marked removed.
#' @export
#'
GetAvailEpisodes <- function(abf) {

  f <- function(x) {
    avail_epi <- GetEpiAvail(x)

    which(avail_epi)
  }

  if (IsAbf(abf)) {
    f(abf)
  } else if (IsAbfList(abf)) {
    lapply(abf, f)
  } else {
    err_class_abf()
  }
}
