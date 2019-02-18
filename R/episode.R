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

#' Mask episodes with a value.
#'
#' @param abf an abf object.
#' @param episodes the episodes to mask.
#' @param value the value assigned to the episodes.
#' @param channel channel id, 1-based.
#'
#' @return an abf object with desired episodes masked with value.
#' @export
#'
MskEpi <- function(abf, episodes, value, channel = 1L) {

  CheckArgs(abf, chan = channel, epi = episodes)

  if (is.na(value)) {
    err_mask_na()
  }

  if (nEpi(abf) == 1L) {
    err_abf_not_episodic()
  }

  abf[, episodes, channel] <- value

  abf
}

#' Mask episodes with a value, by-ref behaviour.
#'
#' @param abf an abf or a list of abf object.
#' @param episodes the episodes to mask.
#' @param value the value assigned to the episodes.
#' @param channel channel id, 1-based.
#'
#' @return an abf object with desired episodes masked with value.
#' @export
#'
MaskEpisodes <- function(abf, episodes, value, channel = 1L) {

  if (IsAbf(abf)) {
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
    err_class_abf()
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

  CheckArgs(abf, epi = episodes)

  if (nEpi(abf) == 1L) {
    err_abf_not_episodic()
  }

  attr(abf, "EpiAvail")[episodes] <- FALSE

  abf
}

#' Remove episodes from an abf object, by-ref behaviour.
#'
#' Function behaviour: episodes will be removed from ALL channels. Removing
#' episodes only affect results of [[ extracting and GetAvailEpisodes, since
#' episodic data is not actually removed but marked "removed"
#'
#' @param abf an abf or a list of abf object.
#' @param episodes episodes to remove.
#'
#' @return an abf object with desired episodes marked removed.
#' @export
#'
RemoveEpisodes <- function(abf, episodes) {

  if (IsAbf(abf)) {
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
    err_class_abf()
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

  CheckArgs(abf, epi = episodes)

  if (nEpi(abf) == 1L) {
    err_abf_not_episodic()
  }

  attr(abf, "EpiAvail")[episodes] <- TRUE

  abf
}

#' Restore previous removed episodes, by-ref behaviour.
#'
#' @param abf an abf or a list of abf object.
#' @param episodes episodes to restore.
#'
#' @return an abf object.
#' @export
#'
RestoreEpisodes <- function(abf, episodes) {

  if (IsAbf(abf)) {
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
