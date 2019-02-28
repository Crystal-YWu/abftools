#' Calculate baseline of a channel
#'
#' @param abf an abf object.
#' @param episode episodes to calculate baseline
#' @param channel channel to calculate baseline
#' @param epoch OPTIONAL, specific epoch to calculate baseline, if NULL all epochs are calculated.
#' @param ... arguments passed to baseline_als
#'
#' @return baseline of the channel
#' @export
#'
ChannelBaseline <- function(abf, episode = GetAllEpisodes(abf), channel = 1L,
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
        mx[mask, i] <- baseline_als(abf[mask, episode[i], channel], ...)
      }
    } else {
      mask <- MaskIntv(epoch_intv[, episode[i], epoch])
      mx[mask, i] <- baseline_als(abf[mask, episode[i], channel], ...)
    }
  }

  epilabel <- DefaultEpiLabel(abf)
  colnames(mx) <- epilabel[episode]

  mx
}

#' Baseline Correction with Asymmetric Least Squares Smoothing
#'
#' Ref: Paul H. C. Eilers, Hans F.M. Boelens, 2005
#'
#' @param y a numeric vector
#' @param lambda large numeric
#' @param p numeric between (0, 1)
#' @param maxitr integer maximum iteration
#' @param converge_warning whether to give a warning if not converged.
#'
#' @return a numeric vector, baseline of y
#' @export
#'
baseline_als <- function(y, lambda = 1e6, p = 0.05, maxitr = 10L,
                         converge_warning = FALSE) {

  n <- length(y)
  diag_idx <- seq_len(n)

  D <- Matrix::diff(Matrix::Diagonal(n), differences = 2)
  Delta <- lambda * Matrix::t(D) %*% D

  w <- rep(1.0, n)
  np <- 1.0 - p
  v <- rep(TRUE, n)
  old_v <- rep(TRUE, n)

  cflag <- TRUE
  for (i in seq_len(maxitr)) {
    #solving sparseMatrix is much faster than Diagonal, W is sparse anyway
    W <- Matrix::sparseMatrix(i = diag_idx, j = diag_idx, x = w)
    z <- as.vector(Matrix::solve(W + Delta, w * y))

    #update w
    v <- y > z
    if (all(v == old_v)) {
      cflag <- FALSE
      break
    }
    old_v <- v

    w <- v * p + (!v) * np
  }

  if (converge_warning && cflag) {
    warning("baseline_als: Convergence not achieved.")
  }

  z
}
