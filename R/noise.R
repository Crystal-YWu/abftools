DenoiseChannel <- function(abf, episode = GetAllEpisodes(abf), channel = 1L,
                           epoch = NULL, ...) {

  channel <- FirstElement(channel)
  CheckArgs(abf, epi = episode, chan = channel, epo = epoch)

  nepo <- nEpoch(abf)
  npts <- nPts(abf)
  nepi <- length(episode)

  if (nepo) {
    epoch_intv <- GetEpochIntervals(abf)
  } else {
    nepo <- 1L
    epoch_intv <- array(c(1L, npts, npts), dim = c(3L, nepi, 1L))
  }

  mx <- abf[, , channel]
  for (i in seq_len(nepi)) {
    if (is.null(epoch)) {
      for (epo in seq_len(nepo)) {
        mask <- MaskIntv(epoch_intv[, episode[i], epo])
        mx[mask, i] <- denoise_wavshrink(abf[mask, episode[i], channel], ...)
      }
    } else {
      mask <- MaskIntv(epoch_intv[, episode[i], epoch])
      mx[mask, i] <- denoise_wavshrink(abf[mask, episode[i], channel], ...)
    }
  }

  mx
}

denoise_wavshrink <- function(y, thresh.scale = 1.0, xform = "dwt", ...) {

  wmtsa::wavShrink(y, thresh.scale = thresh.scale, xform = xform, ...)
}
