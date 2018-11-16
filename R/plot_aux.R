UniYLim <- function(abf_list, channel, intv_list = NULL, blank = 0.0125) {

  ret <- c(0, 0)
  for (i in seq_along(abf_list)) {
    if (is.null(intv_list)) {
      lower <- min(abf_list[[i]][[channel]])
      upper <- max(abf_list[[i]][[channel]])
      delta <- abs(lower - upper)
      lower <- lower - delta * blank
      upper <- upper + delta * blank
    } else {
      #UniYLim can't tell intv_list from cursor_list
      #TODO: Apply class attribute to all intervals passing around?
      #Temp work around
      if (length(intv_list[[i]]) == 3L &&
          (intv_list[[i]][3] == (intv_list[[i]][2] - intv_list[[i]][1] + 1L))) {
        #Interval (possibly)
        mask <- MaskIntv(intv_list[[i]])
      } else {
        #Cursor (possibly)
        mask <- intv_list[[i]]
      }
      lower <- min(abf_list[[i]][[channel]][mask, ])
      upper <- max(abf_list[[i]][[channel]][mask, ])
      delta <- abs(lower - upper)
      lower <- lower - delta * blank
      upper <- upper + delta * blank
    }

    ret <- range(ret, lower, upper)
  }

  return(ret)
}

GetYLimit <- function(abf, intv = NULL, curs = NULL, channel, blank = 0.0125) {

  #convert abf, intv, curs to lists.
  if (IsAbf(abf)) {
    abf <- list(abf)
  }
  if (!is.list(itnv)) {
    intv <- list(intv)
  }
  if (class(curs) != "curs") {
    curs <- list(curs)
  }
  #pad intv and curs
  intv <- rep(intv, length(abf))
  curs <- rep(curs, length(abf))

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
                  us = ctick * GetSamplingIntv(abf),
                  ms = ctick * GetSamplingIntv(abf) / 1000,
                  s  = ctick * GetSamplingIntv(abf) / 1000 / 1000,
                  min = ctick * GetSamplingIntv(abf) / 1000 / 1000 / 60,
                  hr = ctick * GetSamplingIntv(abf) / 1000 / 1000 / 60 / 60,
                  err_time_unit())

  return(ctime)
}
