MeltAbfMean <- function(abflist, intvlist) {

  domean <- function(abf, intv) {
    ret <- mean(abf, intv)
    ret <- cbind(GetTitle(abf), ret)
    colnames(ret)[1] <- "id"

    return(ret)
  }

  melted <- NULL
  if (missing(intvlist) || is.null(intvlist)) {
    intvlist <- rep(NULL, length(abflist))
  }
  for (i in seq_along(abflist)) {
    if (is.null(melted)) {
      melted <- domean(abflist[[i]], intvlist[[i]])
    }
    tmp <- domean(abflist[[i]], intvlist[[i]])
    melted <- rbind(melted, tmp)
  }

  return(melted)
}
