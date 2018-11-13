#' Mask episodes with a value.
#'
#' @param abf an abf object.
#' @param channel channel id, 1-based.
#' @param episodes the episodes to mask.
#' @param value the value assigned to the episodes.
#'
#' @return an abf object with desired episodes masked with value.
#' @export
#'
MskEpi <- function(abf, channel, episodes, value) {

  if (is.na(value)) {
    err_mask_na("MskEpi")
  }

  d <- dim(abf)
  if (d[3] == 1L) {
    err_abf_not_episodic("MskEpi")
  }

  abf[, episodes, channel] <- value

  return(abf)
}

#' Mask episodes with a value, by-ref behaviour.
#'
#' @param abf an abf object.
#' @param channel channel id, 1-based.
#' @param episodes the episodes to mask.
#' @param value the value assigned to the episodes.
#'
#' @return an abf object with desired episodes masked with value.
#' @export
#'
MaskEpisodes <- function(abf, channel, episodes, value) {

  if (is.na(value)) {
    err_mask_na("MaskEpisodes")
  }

  if (class(abf) == "abf") {
    return(
      eval.parent(substitute({
        abf <- MskEpi(abf, channel, episodes, value)
      }))
    )
  } else if (IsAbfList(abf)) {
    warning("MaskEpisodes: masking a list of abf objects.")
    return(
      eval.parent(substitute({
        abf <- lapply(abf, function(x) MskEpi(x, channel, episodes, value))
      }))
    )
  } else {
    err_class_abf_list("MaskEpisodes")
  }

}

#' Remove episodes from an abf object.
#'
#' Function behaviour: episodes will be removed from ALL channels. Removing
#' episodes only affect results of [[ extracting and GetAvailEpisodes, since
#' episodic data is not actually removed but marked "removed"
#'
#' @param abf an abf object.
#' @param episodes episodes to remove.
#'
#' @return an abf object with desired episodes marked removed.
#' @export
#'
RmEpi <- function(abf, episodes) {

  d <- dim(abf)
  if (d[3] == 1L)
    err_abf_not_episodic("RmEpi")

  epi_avail <- attr(abf, "EpiAvail")
  epi_avail[episodes] <- FALSE
  attr(abf, "EpiAvail") <- epi_avail

  return(abf)
}

#' Remove episodes from an abf object, by-ref behaviour.
#'
#' Function behaviour: episodes will be removed from ALL channels. Removing
#' episodes only affect results of [[ extracting and GetAvailEpisodes, since
#' episodic data is not actually removed but marked "removed"
#'
#' @param abf an abf object.
#' @param episodes episodes to remove.
#'
#' @return an abf object with desired episodes marked removed.
#' @export
#'
RemoveEpisodes <- function(abf, episodes) {

  if (class(abf) == "abf") {
    return(
      eval.parent(substitute({
        abf <- RmEpi(abf, episodes)
      }))
    )
  } else if (IsAbfList(abf)) {
    warning("RemoveEpisodes: removing episodes from a list of abf objects.")
    return(
      eval.parent(substitute({
        abf <- lapply(abf, function(x) RmEpi(x, episodes))
      }))
    )
  } else {
    err_class_abf_list("RemoveEpisodes")
  }

}

#' Restore previous removed episodes.
#'
#' @param abf an abf object.
#' @param episodes episodes to restore.
#'
#' @return an abf object.
#' @export
#'
ResEpi <- function(abf, episodes) {

  d <- dim(abf)
  if (d[3] == 1L)
    err_abf_not_episodic("ResEpi")

  epi_avail <- attr(abf, "EpiAvail")
  epi_avail[episodes] <- TRUE
  attr(abf, "EpiAvail") <- epi_avail

  return(abf)
}

#' Restore previous removed episodes, by-ref behaviour.
#'
#' @param abf an abf object.
#' @param episodes episodes to restore.
#'
#' @return an abf object.
#' @export
#'
RestoreEpisodes <- function(abf, episodes) {

  if (class(abf) == "abf") {
    return(
      eval.parent(substitute({
        abf <- ResEpi(abf, episodes)
      }))
    )
  } else if (IsAbfList(abf)) {
    return(
      eval.parent(substitute({
        abf <- lapply(abf, function(x) ResEpi(x, episodes))
      }))
    )
  } else {
    err_class_abf_list("RestoreEpisodes")
  }

}

#' Get available episodes
#'
#' @param abf an abf objects
#'
#' @return episodes that are not marked removed.
#' @export
#'
GetAvailEpisodes <- function(abf) {

  f <- function(x) {
    all_epi <- seq_len(nEpi(x))
    avail_epi <- attr(x, "EpiAvail")

    return(all_epi[avail_epi])
  }

  if (class(abf) == "abf") {
    return(f(abf))
  } else if (IsAbfList(abf)) {
    return(lapply(abf, f))
  } else {
    err_class_abf_list("GetAvailEpisodes")
  }

}
