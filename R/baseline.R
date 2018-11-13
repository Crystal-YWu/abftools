GetBaseline <- function(abf, epoch, intv, episodes, channel = 1, algo = "als", ...) {

  missing_epoch <- missing(epoch)
  missing_intv <- missing(intv)
  if (missing_epoch && missing_intv) {
    err_wrong_arg_num("Expected epoch or intv.")
  }
  if (!xor(missing_intv, missing_epoch)) {
    warning("GetBaseline: argument epoch will be ignored since intv is provided.")
  }

  if (missing_intv) {
    epoch_intv <- GetEpochIntervals(abf)
    intv <- c(1L, 1L, 1L)
    if (is.character(epoch)) {
      epoch <- GetEpochId(epoch)
    }
  }

  mask <- intv[1]:intv[2]

  baseline_f <- paste0("baseline_", algo)
  bl <- list()
  if (missing(episodes)) {
    episodes <- seq_len(nEpi(abf))
  }
  channel <- FirstElement(channel)
  for (i in episodes) {
    if (missing_intv) {
      intv <- epoch_intv[, epoch, i]
      mask <- intv[1]:intv[2]
    }
    y <- abf[mask, i, channel]
    bl[[i]] <- do.call(baseline_f, list(y = y, ...))
  }

  return(bl)
}

#' Calculate baselines of an abf object.
#'
#' Currently only als (Asymmetric Least Squares Smoothing) is available.
#'
#' @param abf an abf object.
#' @param epoch the epoch name/id to evaluate baselines in.
#' @param episodes episodes/sweeps to calculate.
#' @param channel channel id, 1-based.
#' @param algo algorithm to calculate baselines.
#' @param ... other arguments to pass to the selected algorithm.
#'
#' @return baselines of selected episodes/sweeps in a list of vectors
#' @export
#'
BaselineEpoch <- function(abf, epoch, episodes, channel = 1L, algo = "als", ...) {

  if (missing(episodes) || is.null(episodes)) {
    episodes <- seq_len(nEpi(abf))
  }
  baseline <- ExternalAlgoEpoch(abf, epoch, episodes, channel, "baseline", algo, ...)

  return(baseline)
}

#' Calculate baselines of an abf object.
#'
#' Currently only als (Asymmetric Least Squares Smoothing) is available.
#'
#' @param abf an abf object.
#' @param intv the interval to evaluate baselines in.
#' @param episodes episodes/sweeps to calculates.
#' @param channel channel id, 1-based.
#' @param algo algorithm to calculate baselines.
#' @param ... other arguments to pass to the selected algorithm.
#'
#' @return baselines of selected episodes/sweeps in a named column matrix
#' @export
#'
BaselineIntv <- function(abf, intv, episodes, channel = 1L, algo = "als", ...) {

  if (missing(episodes) || is.null(episodes)) {
    episodes <- seq_len(nEpi(abf))
  }
  baseline <- ExternalAlgoIntv(abf, intv, episodes, channel, "baseline", algo, ...)

  return(baseline)
}

#baseline removal from DWT?

###Baseline Correction with Asymmetric Least Squares Smoothing
###Paul H. C. Eilers, Hans F.M. Boelens, 2005
baseline_als <- function(y, lambda_pow10 = 6, p = 0.05, maxitr = 10,
                         converge_warning = FALSE) {

  #Calculate D
  m <- length(y)
  diag_idx <- seq_len(m)

  D <- diff(Matrix::Diagonal(m), differences = 2)
  lambda <- 10^lambda_pow10
  Delta <- lambda * t(D) %*% D

  w <- rep(1, m)
  np <- 1 - p
  old_v <- rep(TRUE, m)
  v <- rep(TRUE, m)

  cflag <- FALSE

  for (i in seq_len(maxitr)) {
    #W <- Diagonal(x = w)
    #solving sparseMatrix is much faster than Diagonal, W is sparse anyway
    W <- Matrix::sparseMatrix(i = diag_idx, j = diag_idx, x = w)

    #Note of as.vector: solved z should be an dgeMatrix, coercing it to a vector
    #greatly improve the speed of the following calculation by vectorisation
    z <- as.vector(Matrix::solve(W + Delta, w * y))

    #update w
    v <- y > z
    if (all(v == old_v)) {
      cflag <- TRUE
      break
    }
    old_v <- v

    #ifelse is painfully slow
    #w <- ifelse(y > z, p, np)
    w <- v * p + (!v) * np
  }

  if (!cflag && converge_warning) {
    warning("baseline_als: Convergence not achieved.")
  }

  return(z)
}
