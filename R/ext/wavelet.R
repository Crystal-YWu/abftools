DenoiseShrink <- function(y, t_scale = 1, ...) {

  return(wavShrink(y, thresh.scale = t_scale, ...))
}

#functions for test purpose only, very slow & opt needed.
DenoiseEpi <- function(abf, episodes, channel, ...) {

  for (epi in episodes) {
    y <- DenoiseShrink(abf[, epi, channel], ...)
    abf[, epi, channel] <- y
  }

  return(abf)
}

DenoiseChan <- function(abf, channel, ...) {

  ch <- abf[,, channel]
  for (i in seq.int(nEpi(abf))) {

    y <- DenoiseShrink(ch[, i], ...)
    ch[, i] <- y
  }
  abf <- RplcChan(abf, channel, ch)

  return(abf)
}

DenoiseAbf <- function(abf, ...) {

  for (i in seq.int(nChan(abf))) {
    abf <- DenoiseChan(abf, i, ...)
  }

  return(abf)
}
