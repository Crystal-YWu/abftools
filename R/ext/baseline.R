#slow function, opt needed
#
GetBaseline <- function(abf, epoch, channel, ...) {

  epoch_intv <- GetEpochIntervals(abf)
  #baseline function accepts a row matrix, however, epoch length might be
  #different, we might not able to simply extract all data points to a matrix.
  #Do in a in efficient way

  get_data_points <- function(epi) {
    intv <- epoch_intv[, epoch, epi]
    mask <- intv[1]:intv[2]
    #baseline only accepts row matrices. Weird.
    return(t(abf[mask, epi, channel]))
  }
  #do all episodes regardless of removed flag
  bl <- list()
  #cr <- list()
  for (i in seq(nEpi(abf))) {
    tmp <- baseline(get_data_points(i), ...)
    bl[[i]] <- getBaseline(tmp)
    #cr[[i]] <- getCorrected(tmp)
  }

  return(bl)
}

#baseline removal from DWT?
