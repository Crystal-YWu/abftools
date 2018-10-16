print.abf <- function(x, ...) {

}

#' Title
#'
#' @param x
#' @param channel
#'
#' @return
#' @export
#'
#' @examples
`[[.abf` <- function(x, channel) {

  d <- dim(x)
  if (length(channel) > 1L)
    stop("Extract channel: you can only select one channel.")
  if (channel > d[1L])
    stop(paste0("Extract channel: channel ", channel, " not available."))

  #when using [[ to extract, we use 0 based index so the channel matches that was
  #set in pClamp software
  channel <- channel + 1L

  epi <- GetAvailEpisodes(x)
  #in case of only one episode
  if (length(epi) == 1L) {
    #1d vector
    df <- x[channel, ,epi]
    #2d matrix
    df <- matrix(df, ncol = 1L)
  } else {
    #2d matrix
    df <- x[channel, , epi]
  }
  colnames(df) <- paste0("epi", epi)

  return(df)
}

#' Title
#'
#' @param x
#' @param channel
#'
#' @return
#' @export
#'
#' @examples
as.data.frame.abf <- function(x, channel = 0L) {

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
`[[<-.abf` <- function(x, channel, ...) {

  stop("Replace method for whole channel is not supported.")
}
