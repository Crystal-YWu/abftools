allowed_delta_abs <- function(channel_data, delta) {

  d <- dim(channel_data)
  ret <- array(data = abs(delta), dim = d)

  return(ret)
}

allowed_delta_rel <- function(channel_data, delta) {

  ret <- abs(channel_data * delta)

  return(ret)
}

OverlapEpisodicIntv <- function(episodic_intv, episodes, npts) {

  ret <- rep(TRUE, npts)
  for (epi in episodes) {
    ret <- ret & IntvToLogi(episodic_intv[[epi]], npts)
  }

  return(LogiToIntv(ret))
}

BinSearchIntv <- function(channel_data, intv, min_intv, f) {

  mask <- MaskIntv(intv)
  ret <- list()
  ret$score <- colMads(channel_data[mask, ])
  ret$intv <- intv

  if (intv[3] %/% 2L <= min_intv) {
    return(ret)
  }

  mid_idx <- (intv[1] + intv[2]) %/% 2L
  intv_l <- c(intv[1], mid_idx, mid_idx - intv[1] + 1L)
  intv_r <- c(mid_idx, intv[2], intv[2] - mid_idx + 1L)

  ret_r <- BinSearchIntv(channel_data, intv_r, min_intv , f)
  if (f(ret_r$score, ret$score)) {
    return(ret_r)
  } else {
    ret_l <- BinSearchIntv(channel_data, intv_l, min_intv, f)
    if (f(ret_l$score, ret$score)) {
      return(ret_l)
    } else {
      return(ret)
    }
  }
}

score_worst_half <- function(s1, best_s) {

  #half length + 1
  n <- length(best_s) %/% 2L + 1L
  o <- order(best_s, decreasing = TRUE)
  mask <- o[seq.int(n)]

  return(all(s1[mask] < best_s[mask]))
}
