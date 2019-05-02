#ported from imlijunda/stencil1d
stencil_coefs <- function(stencil, order) {

  S <- t(sapply(seq.int(0L, length(stencil) - 1L), function(n) stencil^n))
  kron <- rep(0, length(stencil))
  kron[order + 1L] <- factorial(order)

  solve(S, kron)
}

stencil_finite_diff <- function(y, x, stencil, order, coefs = NULL) {

  if (is.null(coefs)) {
    coefs <- stencil_coefs(stencil = stencil, order = order)
  }

  matrixStats::colSums2(sapply(x, function(x) y[x + stencil]) * coefs)
}

slope_spline <- function(y, x) {

  idx_na <- is.na(y)

  if (any(idx_na)) {
    idx <- !idx_na
    #interpSpline() does not support NA values
    spl <- splines::interpSpline(x[idx], y[idx], bSpline = FALSE, period = NULL, ord = 4L)
    dy_dx <- stats::predict(spl, x = x[idx], deriv = 1)
    ans <- rep_len(NA, length(y))
    ans[idx] <- dy_dx$y
    ans
  } else {
    #piecewise poly spline method
    spl <- splines::interpSpline(x, y, bSpline = FALSE, period = NULL, ord = 4L)
    dy_dx <- stats::predict(spl, x = x, deriv = 1)
    dy_dx$y
  }
}

slope_stencil <- function(y, idx, stencil = seq.int(-1, 1)) {

  coefs <- stencil_coefs(stencil = stencil, order = 1L)
  stencil_finite_diff(y, idx, stencil = stencil, order = 1, coefs = coefs)
}
