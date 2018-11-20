MaskIntv <- function(intv) {
  #TODO: A good solution to distinguish intv and curs

  if (length(intv) == 2L) {
    return(intv[1]:intv[2])
  }
  if (length(intv) == 3L && (intv[2] - intv[1] + 1 == intv[3])) {
    return(intv[1]:intv[2])
  }

  return(intv)
}

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
  for (i in seq_len(ncol(intv))) {
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

FirstElement <- function(x) {

  x_name <- deparse(substitute(x))
  if (length(x) > 1L) {
    warning(paste0(x_name,
                   " has length > 1 and only the first element will be used."))
  }

  return(x[[1]])
}

ExtractFrom <- function(abf, epoch, episode, channel, lagL = 0L, lagR = lagL) {

  epoch_intv <- GetEpochIntervals(abf)
  intv <- epoch_intv[, epoch, episode]
  if ((lagL + lagR) < intv[3]) {
    intv[1] <- intv[1] + lagL
    intv[2] <- intv[2] - lagR
  } else {
    #This shouldn't really happen
    add_msg <- sprintf("lag too large: L = %d, R = %d", lagL, lagR)
    err_internal_bug(add_msg)
  }
  mask <- MaskIntv(intv)
  y <- abf[mask, episode, channel]

  return(y)
}

ExpandList <- function(x, ref) {

  if (is.list(x)) {
    if (!AssertLength(x, ref)) {
      return(NULL)
    }
  } else {
    x <- rep(list(x), length(ref))
  }

  return(x)
}

abf_list_copy_paste_place_holder <- function(abf) {

  if (IsAbf(abf)) {

  } else if (IsAbfList(abf)) {

  } else {
    err_class_abf_list()
  }

}
