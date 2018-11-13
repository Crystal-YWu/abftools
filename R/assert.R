IsAbf <- function(x) class(x) == "abf"

IsAbfList <- function(x) {

  if (class(x) != "list")
    return(FALSE)
  else
    return(all(sapply(x, function(y) class(y) == "abf")))
}


AssertLength <- function(x, ...) {

  len <- list(...)
  lenx <- length(x)
  for (i in seq_along(len)) {
    if (lenx == len[[i]]) {
      return(TRUE)
    }
  }

  return(FALSE)
}

AssertChannel <- function(abf, channel) {

  return(nChan(abf) > channel && channel > 0L)
}
