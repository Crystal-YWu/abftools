#' Mask episodes with a value.
#'
#' @param abf an abf object.
#' @param episode the episode to mask.
#' @param value the value assigned to the episodes.
#' @param channel channel id, 1-based.
#'
#' @return an abf object with desired episodes masked with value.
#' @export
#'
MskEpi <- function(abf, episode, value, channel = 1L) {

  CheckArgs(abf, chan = channel, epi = episode)

  if (is.na(value)) {
    err_mask_na()
  }

  if (nEpi(abf) == 1L) {
    err_abf_not_episodic()
  }

  abf[, episode, channel] <- value

  abf
}

#' Mask episodes with a value, by-ref behaviour.
#'
#' @param abf an abf or a list of abf object.
#' @param episode the episodes to mask.
#' @param value the value assigned to the episodes.
#' @param channel channel id, 1-based.
#'
#' @return an abf object with desired episodes masked with value.
#' @export
#'
MaskEpisodes <- function(abf, episode, value, channel = 1L) {

  if (IsAbf(abf)) {
    return(
      eval.parent(substitute({
        abf <- MskEpi(abf, episode = episode, value = value, channel = channel)
      }))
    )
  } else if (IsAbfList(abf)) {
    warning("MaskEpisodes: masking a list of abf objects.")
    return(
      eval.parent(substitute({
        abf <- lapply(abf, function(x) MskEpi(x, episode = episode, value = value, channel = channel))
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
#' @param episode episodes to remove.
#'
#' @return an abf object with desired episodes marked removed.
#' @export
#'
RmEpi <- function(abf, episode) {

  CheckArgs(abf, epi = episode)

  if (nEpi(abf) == 1L) {
    err_abf_not_episodic()
  }

  attr(abf, "EpiAvail")[episode] <- FALSE

  abf
}

#' Remove episodes from an abf object, by-ref behaviour.
#'
#' Function behaviour: episodes will be removed from ALL channels. Removing
#' episodes only affect results of [[ extracting and GetAvailEpisodes, since
#' episodic data is not actually removed but marked "removed"
#'
#' @param abf an abf or a list of abf object.
#' @param episode episodes to remove.
#'
#' @return an abf object with desired episodes marked removed.
#' @export
#'
RemoveEpisodes <- function(abf, episode) {

  if (IsAbf(abf)) {
    return(
      eval.parent(substitute({
        abf <- RmEpi(abf, episode)
      }))
    )
  } else if (IsAbfList(abf)) {
    warning("RemoveEpisodes: removing episodes from a list of abf objects.")
    return(
      eval.parent(substitute({
        abf <- lapply(abf, function(x) RmEpi(x, episode))
      }))
    )
  } else {
    err_class_abf()
  }

}

#' Restore previous removed episodes.
#'
#' @param abf an abf object.
#' @param episode episodes to restore.
#'
#' @return an abf object.
#' @export
#'
ResEpi <- function(abf, episode = NULL) {

  CheckArgs(abf, epi = episode)

  if (nEpi(abf) == 1L) {
    err_abf_not_episodic()
  }

  if (is.null(episode)) {
    episode <- GetAllEpisodes(abf)
  }

  attr(abf, "EpiAvail")[episode] <- TRUE

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
RestoreEpisodes <- function(abf, episode = NULL) {

  if (IsAbf(abf)) {
    return(
      eval.parent(substitute({
        abf <- ResEpi(abf, episode)
      }))
    )
  } else if (IsAbfList(abf)) {
    return(
      eval.parent(substitute({
        abf <- lapply(abf, function(x) ResEpi(x, episode))
      }))
    )
  } else {
    err_class_abf()
  }

}
