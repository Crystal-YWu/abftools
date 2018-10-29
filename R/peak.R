#' Peak detection by epoch.
#'
#' Currently available algorithms: cwt, zscore
#'
#' @param abf an abf object.
#' @param epoch epoch id/name.
#' @param episodes episodes/sweeps to detect peaks.
#' @param channel channel id, 1-based.
#' @param algo algorithm for peak detection.
#' @param ... other argumetns to pass to the selected algorithm.
#'
#' @return a named list of detected peaks' x positions.
#' @export
#'
PeakDetectEpoch <- function(abf, epoch, episodes = 0, channel = 1,
                            algo = "cwt", ...) {

  peaks <- ExternalAlgoEpoch(abf, epoch, episodes, channel, "peak", algo, ...)

  return(peaks)
}

#' Peak detection by epoch.
#'
#' Currently available algorithms: cwt, zscore
#'
#' @param abf an abf object.
#' @param intv an interval to detect peaks.
#' @param episodes episodes/sweeps to detect peaks.
#' @param channel channel id, 1-based.
#' @param algo algorithm for peak detection.
#' @param ... other argumetns to pass to the selected algorithm.
#'
#' @return a named list of detected peaks' x positions.
#' @export
#'
PeakDetectIntv <- function(abf, intv, episodes = 0, channel = 1, algo = "cwt", ...) {

  peaks <- ExternalAlgoIntv_list(abf, intv, episodes, channel, "peak", algo, ...)

  return(peaks)
}

#Smoothed z-score peak detection
#Ref: https://stackoverflow.com/a/22640362
#Does not feel as robust as CWT method
peak_zscore <- function(y, lag = length(y) %/% 1000L, threshold = 2.6, influence = 0) {
  # threshold = 2.6 ~ 1% normal dist
  # influence = 0 assume stational

  npts <- length(y)
  signals <- rep(0L, npts)
  filteredY <- y
  avgFilter <- rep(0, npts)
  stdFilter <- rep(0, npts)

  #Moving window -> compare z-score -> update threshold
  avgFilter[lag] <- mean(y[1:lag])
  stdFilter[lag] <- stats::sd(y[1:lag])
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
    stdFilter[i] <- stats::sd(filteredY[(i - lag):i])
  }

  return(signals)
}

#Pan Du, Warren A. Kibbe, and Simon M. Lin, Bioinformatics, 22, 2059–2065 (2006)
#already implementedin package wmtsa
peak_cwt <- function(y, peak_scale = 0.65, ...) {

  cwt <- wmtsa::wavCWT(y)
  cwt_t <- wmtsa::wavCWTTree(cwt)
  t_hist <- attr(cwt_t, "branch.hist")
  t_scal <- attr(cwt_t, "scale")
  scale_range <- t_scal[range(which(t_hist > quantile(t_hist, prob = peak_scale)))]
  cwt_p <- wmtsa::wavCWTPeaks(cwt_t, scale.range = scale_range, ...)

  return(cwt_p)
}
