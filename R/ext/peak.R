#Smoothed z-score peak detection
#Ref: https://stackoverflow.com/a/22640362
#Moving window -> compare z-score -> update threshold
#Does not feel as robust as CWT method
ThresholdingAlgo <- function(y, lag = length(y) %/% 100L,
                             threshold = 2.6,
                             influence = 0) {
  # threshold = 2.6 ~ 1% normal dist
  # influence = 0 assume stational

  npts <- length(y)
  signals <- rep(0L, npts)
  filteredY <- y
  avgFilter <- rep(0, npts)
  stdFilter <- rep(0, npts)

  avgFilter[lag] <- mean(y[1:lag])
  stdFilter[lag] <- sd(y[1:lag])
  for (i in (lag + 1):npts) {
    if (abs(y[i] - avgFilter[i - 1]) > threshold * stdFilter[i - 1]) {
      if (y[i] > avgFilter[i - 1]) {
        signals[i] <- 1L
      } else {
        signals[i] <- -1L
      }
      filteredY[i] <- influence * y[i] + (1 - influence) * filteredY[i - 1]
    } else {
      signals[i] <- 0
      filteredY[i] <- y[i]
    }
    #seems we can implement a variable window to improve robustness?
    #tests needed.
    avgFilter[i] <- mean(filteredY[(i - lag):i])
    stdFilter[i] <- sd(filteredY[(i - lag):i])
  }

  return(signals)
}

#Pan Du, Warren A. Kibbe, and Simon M. Lin, Bioinformatics, 22, 2059–2065 (2006)
#implemented already in package wmtsa
#wmtsa provides a set of useful functions for wavelet analysis, however, the
#DWT tranform object is too complicated to manipulate.
#Maybe another package or implement our own DWT methods?
#TODO: DWT packages/DWT methods
CWTAlgo <- function(y, peak_scale = 0.65, ...) {

  cwt <- wavCWT(y)
  cwt_t <- wavCWTTree(cwt)
  t_hist <- attr(cwt_t, "branch.hist")
  t_scal <- attr(cwt_t, "scale")
  scale_range <- t_scal[range(which(t_hist > quantile(t_hist, prob = peak_scale)))]
  cwt_p <- wavCWTPeaks(cwt_t, scale.range = scale_range, ...)

  return(cwt_p)
}
