allowed_delta_abs <- function(channel_data, delta) {

  d <- dim(channel_data)
  if (is.null(d)) {
    ans <- rep_len(delta, length.out = length(channel_data))
  } else {
    delta <- rep_len(delta, length.out = d[2])
    ans <- matrix(data = delta, nrow = d[1], ncol = d[2], byrow = TRUE)
  }

  ans
}

OverlapEpisodicIntv <- function(episodic_intv, npts) {

  ret <- rep(TRUE, npts)
  for (intv in episodic_intv) {
    if (!is.null(intv)) {
      ret <- ret & IntvToLogi(intv, npts)
    }
  }

  LogiToIntv(ret)
}

BinSearchIntv <- function(channel_data, intv, min_intv) {

  mask <- MaskIntv(intv)
  ret <- list()
  ret$score <- mean(channel_data[mask])
  ret$intv <- intv

  if (intv[3] %/% 2L <= min_intv) {
    return(ret)
  }

  mid_idx <- (intv[1] + intv[2]) %/% 2L
  intv_l <- Intv(intv[1], mid_idx)
  intv_r <- Intv(mid_idx, intv[2])

  ret_r <- BinSearchIntv(channel_data, intv_r, min_intv)
  ret_l <- BinSearchIntv(channel_data, intv_l, min_intv)

  switch(which.min(c(ret$score,
                     ret_l$score,
                     ret_r$score)),
         ret,
         ret_l,
         ret_r)
}
