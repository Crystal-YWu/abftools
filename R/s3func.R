#' Print an abf object
#'
#' @param x an abf object.
#'
#' @return the abf object itself, invisibly.
#' @export
#' @method print abf
#'
print.abf <- function(x, ...) {

    fmt_str <- paste0("Mode %d abf object %s.\n",
                      "Recorded %d ADC channels of %d episodes with %d points per episode. ",
                      "Sampling interval: %.2f us.")
    s <- sprintf(fmt_str, GetMode(x), GetTitle(x), nChan(x), nEpi(x), nPts(x),
                 GetSamplingIntv(x))
    cat(s, "\n")

    invisible(x)
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
#' @param x an abf object.
#' @param channel ADC channel id, 1-based.
#'
#' @return a matrix of the channel data, with colnames of epiX.
#' @export
#'
`[[.abf` <- function(x, channel) {

  channel <- FirstElement(channel)

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

#' Convert a channel of an abf object to data.frame
#'
#' @param x an abf object.
#' @param channel ADC channel id, 1-based.
#'
#' @return a data frame of the channel data, with colname of epiX.
#' @export
#'
as.data.frame.abf <- function(x, channel = 1) {

  df <- x[[channel]]

  return(data.frame(df))
}

#' Replacing channel data.
#'
#' @param x an abf object.
#' @param channel ADC channel id, 1-based.
#' @param value channel data to replace the original.
#'
#' @return an abf object with the replaced channel.
#' @export
#'
`[[<-.abf` <- function(x, channel, value) {

  return(RplcChan(x, channel, value))
}
