#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
GetNumOfChannel <- function(abf) {

  meta <- get_meta(abf)
  #Every observation of table ADC is a channel
  ret <- nrow(meta$ADC)

  return(ret)
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
GetEpisodesPerChannel <- function(abf) {

  meta <- get_meta(abf)
  ret <- meta$Protocol$lEpisodesPerRun

  return(ret)
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
GetPointsPerEpisode <- function(abf) {

  mode <- GetMode(abf)
  if (mode == 3L) {
    ret <- dim(abf)[2]
  } else {
    meta <- get_meta(abf)
    ret <- meta$Protocol$lNumSamplesPerEpisode %/% GetNumOfChannel(abf)
  }

  return(ret)
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
nChan <- function(abf) {

  return(GetNumOfChannel(abf))
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
nPts <- function(abf) {

  return(GetPointsPerEpisode(abf))
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
nEpi <- function(abf) {

  return(GetEpisodesPerChannel(abf))
}