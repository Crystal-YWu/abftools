FindPeaks <- function(abf, episode = GetAllEpisodes(abf), channel = 1L, peak_prob = 0.85, ...) {

  channel <- FirstElement(channel)
  CheckArgs(abf, epi = episode, chan = channel)

  nepo <- nEpoch(abf)
  npts <- nPts(abf)
  nepi <- length(episode)

  if (nepo) {
    epoch_intv <- GetEpochIntervals(abf)
  } else {
    nepo <- 1L
    epoch_intv <- array(c(1L, npts, npts), dim = c(3L, nepi, 1L))
  }

  ans <- list()
  for (i in seq_len(nepi)) {
    browser()
    x <- c()
    y <- c()
    for (epoch in seq_len(nepo)) {
      intv <- epoch_intv[, episode[i], epoch]
      mask <- MaskIntv(intv)
      peak <- peak_cwt(abf[mask, episode[i], channel], peak_prob = peak_prob, ...)
      x <- c(x, peak$x + intv[1] - 1L)
      y <- c(y, peak$y)
    }
    ans[[episode[i]]] <- list(
      tick = x,
      peak = y
    )
  }

  ans
}

#Pan Du, Warren A. Kibbe, and Simon M. Lin, Bioinformatics, 22, 2059–2065 (2006)
#already implemented in package wmtsa
peak_cwt <- function(y, peak_prob = 0.85, ...) {

  cwt_t <- wmtsa::wavCWTTree(wmtsa::wavCWT(y))
  t_hist <- attr(cwt_t, "branch.hist")
  t_scal <- attr(cwt_t, "scale")
  scale_range <- t_scal[range(which(t_hist > quantile(t_hist, prob = peak_prob)))]

  wmtsa::wavCWTPeaks(cwt_t, scale.range = scale_range, ...)
}
