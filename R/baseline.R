#' Calculate baselines of an abf object
#'
#' Currently only als (Asymmetric Least Squares Smoothing) is available.
#'
#' @param abf an abf object
#' @param epoch an epoch name/id to of which the baselines will be calculated
#' @param intv an interval in which the baselines will be calculated
#' @param episodes episodes to calculate
#' @param channel channel id, 1-based
#' @param algo algorithm to calculate baselines
#' @param ... other arguments to pass to the algorithm
#'
#' @return a list of vectors
#' @export
#'
#' @examples
GetBaseline <- function(abf, epoch, intv, episodes = 0, channel = 1, algo = "als", ...) {

  missing_epoch <- missing(epoch)
  missing_intv <- missing(intv)
  if (missing_epoch && missing_intv) {
    err_wrong_arg_num("GetBaseline", "Expected epoch or intv.")
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
  y <- abf[mask, , channel]

  baseline_f <- paste0("baseline_", algo)
  bl <- list()
  if (episodes[1] == 0) {
    episodes <- seq.int(nEpi(abf))
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

#baseline removal from DWT?

###Baseline Correction with Asymmetric Least Squares Smoothing
###Paul H. C. Eilers, Hans F.M. Boelens, 2005
baseline_als <- function(y, lambda_pow10 = 6, p = 0.05, maxitr = 10,
                         converge_warning = FALSE) {

  #Calculate D
  m <- length(y)
  diag_idx <- seq.int(m)

  D <- diff(Diagonal(m), differences = 2)
  lambda <- 10^lambda_pow10
  Delta <- lambda * t(D) %*% D

  w <- rep(1, m)
  np <- 1 - p
  old_v <- rep(TRUE, m)
  v <- rep(TRUE, m)

  cflag <- FALSE

  for (i in seq.int(maxitr)) {
    #W <- Diagonal(x = w)
    #solving sparseMatrix is much faster than Diagonal, W is sparse anyway
    W <- sparseMatrix(i = diag_idx, j = diag_idx, x = w)

    #Note of as.vector: solved z should be an dgeMatrix, coercing it to a vector
    #greatly improve the speed of the following calculation by vectorisation
    z <- as.vector(solve(W + Delta, w * y))

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
