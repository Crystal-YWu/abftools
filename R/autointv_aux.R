allowed_delta_abs <- function(channel_data, delta) {

  d <- dim(channel_data)

  episode <- d[2]
  if (length(delta) != 1L && length(delta) != episode) {
    err_assert_len(delta, episode)
  }

  ret <- matrix(data = abs(delta), nrow = d[1], ncol = d[2], byrow = TRUE)
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
  ret$score <- matrixStats::colMads(channel_data[mask, ])
  ret$intv <- intv

  if (intv[3] %/% 2L <= min_intv) {
    return(ret)
  }

  mid_idx <- (intv[1] + intv[2]) %/% 2L
  intv_l <- Intv(intv[1], mid_idx)
  intv_r <- Intv(mid_idx, intv[2])

  ret_r <- BinSearchIntv(channel_data, intv_r, min_intv , f)
  ret_l <- BinSearchIntv(channel_data, intv_l, min_intv, f)
  if (f(ret_r$score, ret$score)) {
    if (f(ret_l$score, ret_r$score)) {
      return(ret_l)
    } else {
      return(ret_r)
    }
  } else {
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
  mask <- o[seq_len(n)]

  return(all(s1[mask] <= best_s[mask]))
}

score_all <- function(s1, best_s) {

  return(all(s1 <= best_s))
}
