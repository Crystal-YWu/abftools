print.abf <- function(x, ...) {

}

#' Extract channel data from an abf object.
#'
#' In an abf object, channel id is one-based instead of zero-based as in pClamp
#' software. The decision is made to maintain programming consistency since R data
#' is one-based. The significance is you may find ADC channel names not correspond
#' to channel id i.e. channel 1 is "IN 0", channel 2 is "IN 1" instead. This may
#' also apply to other properties such as DAC num, Epoch num etc which have
#' internal numbered indices.
#'
#' @param x
#' @param channel
#'
#' @return
#' @export
#'
#' @examples
`[[.abf` <- function(x, channel) {

  channel <- first_elem(channel)

  d <- dim(x)
  if (channel > d[3])
    stop(paste0("Extract channel: channel ", channel, " not available."))

  epi <- GetAvailEpisodes(x)
  #in case of only one episode
  if (length(epi) == 1) {
    #1d vector
    df <- x[, epi, channel]
    #2d matrix
    df <- matrix(df, ncol = 1)
  } else if (AllEpisodesAvail(x)) {
    #2d matrix
    df <- x[, , channel]
  } else {
    #2d matrix
    df <- x[, epi, channel]
  }
  colnames(df) <- paste0("epi", epi)

  return(df)
}
AllEpisodesAvail <- function(abf) all(attr(abf, "EpiAvail"))

#' Title
#'
#' @param x
#' @param channel
#'
#' @return
#' @export
#'
#' @examples
as.data.frame.abf <- function(x, channel = 1) {

  df <- x[[channel]]

  return(data.frame(df))
}

#' Title
#'
#' @param x
#' @param channel
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
`[[<-.abf` <- function(x, ...) {

  stop("Replace method for whole channel is not supported.")
}
