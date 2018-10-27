GetBaseline <- function(abf, epoch, channel, ...) {

  epoch_intv <- GetEpochIntervals(abf)
  get_data_points <- function(epi) {
    intv <- epoch_intv[, epoch, epi]
    mask <- intv[1]:intv[2]
    return(abf[mask, epi, channel])
  }

  #do all episodes regardless of removed flag
  bl <- list()
  for (i in seq(nEpi(abf))) {
    bl[[i]] <- baseline_als(get_data_points(i), ...)
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
