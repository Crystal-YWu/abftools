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
  if (length(channel) > 1)
    stop("Extract channel: you can only select one channel.")
  if (channel > d[1])
    stop(paste0("Extract channel: channel ", channel, " not available."))

  epi <- GetAvailEpisodes(x)
  #in case of only one episode
  if (length(epi) == 1) {
    #1d vector
    df <- x[channel, ,epi]
    #2d matrix
    df <- matrix(df, ncol = 1)
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
`[[<-.abf` <- function(x, channel, ...) {

  stop("Replace method for whole channel is not supported.")
}
