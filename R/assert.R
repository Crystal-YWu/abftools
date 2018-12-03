IsAbf <- function(x) class(x) == "abf"

IsAbfList <- function(x) {

  IsListOf(x, "abf")
}

IsListOf <- function(x, cls) {

  if (!is.list(x))
    return(FALSE)
  else
    return(all(sapply(x, function(item) all(cls %in% class(item)))))
}

AssertLength <- function(x, ..., explicit = NULL) {

  args <- list(...)
  lenx <- length(x)
  if (any(lenx == explicit)) {
    return(TRUE)
  }
  for (i in seq_along(args)) {
    len <- length(args[[i]])
    if (lenx == len) {
      return(TRUE)
    }
  }

  return(FALSE)
}

AssertEpisode <- function(abf, episode) {

  return(all(nEpi(abf) >= episode & episode > 0L))
}

AssertChannel <- function(abf, channel) {

  return(all(nChan(abf) >= channel & channel > 0L))
}

AssertEpoch <- function(abf, epoch) {

  return(all(nEpoch(abf) >= epoch & epoch > 0L))
}
