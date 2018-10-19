#Is this possible to optimise?
within_interval <- function(x, intv) intv[1] <= x && x <= intv[2]

MaskIntv <- function(intv) intv[1]:intv[2]

OvlpIntv <- function(intv1, intv2) {

  idx1 <- max(intv1[1], intv2[1])
  idx2 <- min(intv1[2], intv2[2])
  len <- idx2 - idx1 + 1L

  if (len <= 0) {
    return(NA)
  } else {
    return(c(idx1, idx2, len))
  }
}

ShftIntv <- function(intv, pts) {

  intv[1] <- intv[1] + pts
  intv[2] <- intv[2] + pts

  return(intv)
}

FilterMinIntervalSize <- function(interval, min_intv_size) {

  mask <- min_intv_size <= interval[, 3]

  return(interval[mask, , drop = FALSE])
}

FilterMaxIntervalSize <- function(interval, max_intv_size) {

  mask <- interval[, 3] <= max_intv_size

  return(interval[mask, , drop = FALSE])
}

FilterMinIntervalPos <- function(interval, min_intv_pos) {

  mask <- min_intv_pos <= interval[, 1]

  return(interval[mask, , drop = FALSE])
}

FilterMaxIntervalPos <- function(interval, max_intv_pos) {

  mask <- interval[, 2] <= max_intv_pos

  return(interval[mask, , drop = FALSE])
}

