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
  if (length(d) == 2)
    return(x[channel, ])

  #episodic
  epi <- AvailEpisodes(x)
  df <- x[channel, , epi]
  colnames(df) <- paste0("epi", epi)

  return(df)
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
