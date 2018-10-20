#' Title
#'
#' @param abf
#' @param channel
#' @param episodes
#' @param value
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param abf
#' @param channel
#' @param episodes
#' @param value
#'
#' @return
#' @export
#'
#' @examples
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
        for (i_____ in seq_along(abf)) {
          abf[[i_____]] <- MskEpi(abf[[i_____]], channel, episodes, value)
        }
        rm(i_____)
        #invisible so this function still "returns" a value
        invisible(abf)
      }))
    )
  } else {
    err_class_abf_list("MaskEpisodes")
  }

}

#' Title
#'
#' @param abf
#' @param episodes
#'
#' @return
#' @export
#'
#' @examples
RmEpi <- function(abf, episodes) {

  d <- dim(abf)
  if (d[3] == 1L)
    err_abf_not_episodic("RmEpi")

  epi_avail <- attr(abf, "EpiAvail")
  epi_avail[episodes] <- FALSE
  attr(abf, "EpiAvail") <- epi_avail

  return(abf)
}

#' Title
#'
#' @param abf
#' @param episodes
#'
#' @return
#' @export
#'
#' @examples
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
        for (i_____ in seq_along(abf)) {
          abf[[i_____]] <- RmEpi(abf[[i_____]], episodes)
        }
        rm(i_____)
        #invisible so this function still "returns" a value
        invisible(abf)
      }))
    )
  } else {
    err_class_abf_list("RemoveEpisodes")
  }

}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
GetAvailEpisodes <- function(abf) {

  f <- function(x) {
    all_epi <- seq.int(nEpi(x))
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
