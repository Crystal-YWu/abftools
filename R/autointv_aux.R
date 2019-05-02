OverlapEpisodicIntv <- function(episodic_intv, npts) {

  ret <- rep(TRUE, npts)
  for (intv in episodic_intv) {
    if (!is.null(intv)) {
      ret <- ret & IntvToLogi(intv, npts)
    }
  }

  LogiToIntv(ret)
}

td_penalty <- function(data, idx){

  data[idx] <- data[idx] * exp(min(idx) / 2.0 / idx)

  data
}

BinSearchIntv <- function(channel_data, intv, min_intv,
                          idx_min = NULL, idx_max = NULL) {

  if (is.null(idx_min)) {
    idx_min <- intv[1]
  }
  if (is.null(idx_max)) {
    idx_max <- intv[2]
  }

  #perturbation 20% left/right
  p_len <- intv[3] %/% 5L
  intv_l <- ShftIntv(intv, -p_len, idx_min = idx_min, idx_max = idx_max)
  intv_r <- ShftIntv(intv, p_len, idx_min = idx_min, idx_max = idx_max)
  score_l <- mean(channel_data[MaskIntv(intv_l)])
  score_r <- mean(channel_data[MaskIntv(intv_r)])
  score <- mean(channel_data[MaskIntv(intv)])

  ret <- switch(which.min(c(score,
                            score_l,
                            score_r)),
                list(score = score, intv = intv),
                list(score = score_l, intv = intv_l),
                list(score = score_r, intv = intv_r))

  if (intv[3] %/% 2L <= min_intv) {
    return(ret)
  }

  mid_idx <- (intv[1] + intv[2]) %/% 2L
  intv_l <- Intv(intv[1], mid_idx)
  intv_r <- Intv(mid_idx, intv[2])

  ret_r <- BinSearchIntv(channel_data, intv_r, min_intv, idx_min, idx_max)
  ret_l <- BinSearchIntv(channel_data, intv_l, min_intv, idx_min, idx_max)

  switch(which.min(c(ret$score,
                     ret_l$score,
                     ret_r$score)),
         ret,
         ret_l,
         ret_r)
}
