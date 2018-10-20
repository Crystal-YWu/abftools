UniYLim <- function(abf_list, channel, intv_list = NULL) {

  ret <- c(0, 0)
  for (i in seq_along(abf_list)) {
    if (is.null(intv_list)) {
      lower <- min(abf_list[[i]][[channel]])
      upper <- max(abf_list[[i]][[channel]])
      delta <- abs(lower - upper)
      lower <- lower - delta * 0.0125
      upper <- upper + delta * 0.0125
    } else {
      focus <- (intv_list[[i]][1] + intv_list[[i]][2]) %/% 2L
      val <- sort(as.vector(abf_list[[i]][channel, focus, ]))
      lower <- val[1] - abs(val[1] - val[2]) * 0.5
      n <- length(val)
      upper <- val[n] + abs(val[n] - val[n - 1L]) * 0.5
    }

    ret <- range(ret, lower, upper)
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
                  err_time_unit("TickToTime"))

  return(ctime)
}
