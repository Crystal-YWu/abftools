#' Calculate baseline of an episode
#'
#' @param abf an abf object.
#' @param episode an episode to calculate baseline
#' @param channel channel to calculate baseline
#' @param by_epoch calculate baselines by epochs, if set to FALSE, the whole episode
#' is used to calculate baselines, otherwise, baselines are calculated piece-by-piece
#' by each epoch and then concatenated.
#' @param dac dac channel if by_epoch is used.
#' @param ... arguments passed to baseline_als, see \code{\link[abftools:baseline_als]{baseline_als()}} for details.
#'
#' @return baseline of the channel
#' @export
#'
EpisodeBaseline <- function(abf, episode, channel, by_epoch = TRUE, dac = GetWaveformEnabledDAC(abf), ...) {

  episode <- FirstElement(episode)
  channel <- FirstElement(channel)

  if (by_epoch) {
    CheckArgs(abf, epi = episode, chan = channel, dac = dac)

    epo <- GetEpochIntervals(abf, dac = dac)
    intvs <- epo[, episode, ]
    n_intvs <- ncol(intvs)
    baselines <- lapply(seq_len(n_intvs), function(i) {
      mask <- MaskIntv(intvs[, i])
      y <- abf[mask, episode, channel]
      baseline_als(y, ...)
    })

    intv1 <- Intv(startPos = 1L, endPos = intvs[1, 1] - 1L)
    mask <- MaskIntv(intv1)
    h1 <- abf[mask, episode, channel]

    intv2 <- Intv(startPos = intvs[2, n_intvs] + 1L, endPos = nPts(abf))
    mask <- MaskIntv(intv2)
    h2 <- abf[mask, episode, channel]

    c(h1, unlist(baselines), h2)

  } else {
    CheckArgs(abf, epi = episode, chan = channel)

    y <- abf[, episode, channel]
    baseline_als(y, ...)

  }
}

#' Remove baseline of a channel.
#'
#' @details Avaiable methods for baseline removal are holding, interval and episode.
#' The interval method evaluate mean baseline level from the given episode between intv,
#' then subtract the value to every episode.
#' The episode method calculates baseline by calling \code{\link[abftools:baseline_als]{baseline_als()}}.
#' The holding method simply read holding level from DAC setting and subtract the
#' value to every episode. This is usually desirable only for waveform channel.
#'
#' @param abf an abf object.
#' @param channel a channel to perform baseline removal
#' @param method method to use. See details.
#' @param intv used in interval method. An interval to evaluate baseline level.
#' @param episode used in episode method. The episode to evaluate baseline.
#' @param by_epoch used in episode method, see \code{\link[abftools:EpisodeBaseline]{EpisodeBaseline()}} for details.
#' @param dac used in holding and episode method, waveform dac channel.
#' @param ... used in episode method, passed to baseline_als, see \code{\link[abftools:baseline_als]{baseline_als()}} for details.
#'
#' @return an abf object.
#' @export
#'
RemoveBaseline <- function(abf, channel, method = c("interval", "episode", "holding"),
                           intv, episode, by_epoch = TRUE, dac = GetWaveformEnabledDAC(abf),
                           ...) {

  method <- match.arg(method)

  channel <- FirstElement(channel)

  switch(method,
         holding = CheckArgs(abf, chan = channel, dac = dac),
         interval = CheckArgs(abf, chan = channel),
         episode = CheckArgs(abf, epi = episode, chan = channel, dac = dac))

  bl <- switch(method,
               holding = {
                 meta <- get_meta(abf)
                 meta$DAC$fDACHoldingLevel[dac]
               },
               interval = {
                 mask <- MaskIntv(intv)
                 tmp <- mapnd(abf[mask,, channel, drop = FALSE], func = mean)
                 npts <- nPts(abf)
                 matrix(data = tmp, nrow = npts, ncol = length(tmp), byrow = TRUE)
               },
               episode = {
                 episode <- FirstElement(episode)
                 EpisodeBaseline(abf, episode = episode, channel = channel,
                                 by_epoch = by_epoch, dac = dac, ...)
               })
  abf[,, channel] <- abf[,, channel] - bl

  abf
}

#' Baseline Correction with Asymmetric Least Squares Smoothing
#'
#' @param y a numeric vector, signals.
#' @param lambda a large numeric, determines smoothness.
#' @param p a numeric between (0, 1), determines asymeetry.
#' @param maxitr an integer, maximum iteration.
#' @param converge_warning whether to give a warning if not converged.
#'
#' @references Paul H. C. Eilers, Hans F.M. Boelens, 2005
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
