GetYLimit <- function(abf, intv = NULL, curs = NULL, channel, blank = 0.0125) {

  #convert abf, intv, curs to lists.
  if (IsAbf(abf)) {
    abf <- list(abf)
  }
  #pad intv and curs
  if (!is.list(intv)) {
    intv <- list(intv)
  }
  if (!is.list(curs)) {
    curs <- list(curs)
  }

  ret <- NULL
  for (i in seq_along(abf)) {

    #get pts of interest
    poi <- c(intv[[i]][1:2], curs[[i]])
    if (is.null(poi)) {
      #no intv or curs is given, set mask to whole channel
      mask <- seq_len(nPts(abf[[i]]))
    } else {
      xrange <- range(poi)
      mask <- seq(xrange[1], xrange[2])
    }

    lower <- min(abf[[i]][[channel]][mask, ])
    upper <- max(abf[[i]][[channel]][mask, ])
    delta <- abs(lower - upper)
    lower <- lower - delta * blank
    upper <- upper + delta * blank

    ret <- range(ret, upper, lower)
  }

  return(ret)
}

TickToTime <- function(abf, time_unit, ctick) {

  ctime <- switch(time_unit,
                  tick = ctick,
                  us =  (ctick - 1L) * GetSamplingIntv(abf),
                  ms =  (ctick - 1L) * GetSamplingIntv(abf) / 1000,
                  s  =  (ctick - 1L) * GetSamplingIntv(abf) / 1000 / 1000,
                  min = (ctick - 1L) * GetSamplingIntv(abf) / 1000 / 1000 / 60,
                  hr =  (ctick - 1L) * GetSamplingIntv(abf) / 1000 / 1000 / 60 / 60,
                  err_time_unit())

  return(ctime)
}

GetAxisLabel <- function(desc, unit) paste0(desc, " (", unit, ")")
