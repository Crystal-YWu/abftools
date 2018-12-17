#' Print an abf object
#'
#' @param x an abf object.
#' @param ... not used.
#'
#' @return the abf object itself, invisibly.
#' @export
#' @method print abf
#'
print.abf <- function(x, ...) {

  mode <- GetMode(x)
  if (mode == 1L) {
    fmt_str <- paste0("Mode %d abf object %s.\n",
                      "Recorded %d ADC channels and %d events. ",
                      "Sampling interval: %.2f us.")
    s <- sprintf(fmt_str, mode, GetTitle(x), nChan(x), nEpi(x), GetSamplingIntv(x))
  } else {
    fmt_str <- paste0("Mode %d abf object %s.\n",
                      "Recorded %d ADC channels of %d episodes with %d points per episode. ",
                      "Sampling interval: %.2f us.")
    s <- sprintf(fmt_str, mode, GetTitle(x), nChan(x), nEpi(x), nPts(x),
                 GetSamplingIntv(x))
  }
  print(s)

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

  if (!AssertChannel(x, channel)) {
    err_channel()
  }

  epi <- GetAvailEpisodes(x)
  epi_names <- DefaultEpiLabel(x)

  npts <- nPts(x)
  nepi <- length(epi)

  mx <- x[, epi, channel]
  dim(mx) <- c(npts, nepi)
  colnames(mx) <- epi_names[epi]

  return(mx)
}
AllEpisodesAvail <- function(abf) all(attr(abf, "EpiAvail"))

#' Convert a channel of an abf object to data.frame
#'
#' @param x an abf object.
#' @param row.names see as.data.frame
#' @param optional see as.data.frame
#' @param channel ADC channel id, 1-based.
#' @param ... passed to as.data.frame
#'
#' @return a data frame of the channel data, with colname of epiX.
#' @export
#'
as.data.frame.abf <- function(x, row.names = NULL, optional = FALSE, channel = 1, ...) {

  mx <- x[[channel]]

  return(as.data.frame(mx, row.names, optional, ...))
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
