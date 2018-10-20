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

LogiToIntv <- function(logi) {

  r <- rle(logi)
  idx_end <- cumsum(r$lengths)
  idx_start <- idx_end - r$lengths + 1
  #exploit that logi is logical
  idx_start <- idx_start[r$values]
  idx_end <- idx_end[r$values]
  win_length <- r$lengths[r$values]

  win <- rbind(idx_start, idx_end, win_length)
  return(win)
}

IntvToLogi <- function(intv, full_length) {

  ret <- rep(FALSE, full_length)
  for (i in seq.int(ncol(intv))) {
    mask <- MaskIntv(intv[, i])
    ret[mask] <- TRUE
  }

  return(ret)
}

FilterMinIntervalSize <- function(interval, min_intv_size) {

  mask <- min_intv_size <= interval[3, ]

  return(interval[ , mask, drop = FALSE])
}

FilterMaxIntervalSize <- function(interval, max_intv_size) {

  mask <- interval[3, ] <= max_intv_size

  return(interval[ , mask, drop = FALSE])
}

FilterMinIntervalPos <- function(interval, min_intv_pos) {

  mask <- min_intv_pos <= interval[1, ]

  return(interval[ , mask, drop = FALSE])
}

FilterMaxIntervalPos <- function(interval, max_intv_pos) {

  mask <- interval[2, ] <= max_intv_pos

  return(interval[ , mask, drop = FALSE])
}

IsAbfList <- function(x) {

  if (class(x) != "list")
    return(FALSE)
  else
    return(all(sapply(x, function(y) class(y) == "abf")))
}

FirstElement <- function(x) {

  x_name <- deparse(substitute(x))
  if (length(x) > 1L) {
    warning(paste0(x_name,
                   " has length > 1 and only the first element will be used."))
  }

  return(x[1])
}

abf_list_copy_paste_place_holder <- function(abf) {

  if (class(abf) == "abf") {

  } else if (IsAbfList(abf)) {

  } else {
    err_class_abf_list("")
  }

}
