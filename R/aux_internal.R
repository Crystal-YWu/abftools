#' Mask a time interval
#'
#' @param intv
#'
#' @return an integer vector
#' @export
#'
MaskIntv <- function(intv) {
  #TODO: A good solution to distinguish intv and curs

  if (length(intv) == 2L) {
    return(seq.int(intv[1], intv[2]))
  }
  if (length(intv) == 3L && (intv[2] - intv[1] + 1L == intv[3])) {
    return(seq.int(intv[1], intv[2]))
  }

  intv
}

OvlpIntv <- function(intv1, intv2) {

  idx1 <- max(intv1[1], intv2[1])
  idx2 <- min(intv1[2], intv2[2])
  len <- idx2 - idx1 + 1L

  if (len <= 0) {
    NA
  } else {
    Intv(idx1, idx2, len)
  }
}

ShftIntv <- function(intv, pts, idx_min = 1L, idx_max = NULL) {

  idx1 <- intv[1] + pts
  idx2 <- intv[2] + pts

  if (!is.null(idx_min)) {
    idx1 <- max(idx1, idx_min)
    idx2 <- max(idx2, idx_min)
  }
  if (!is.null(idx_max)) {
    idx1 <- min(idx1, idx_max)
    idx2 <- min(idx2, idx_max)
  }

  Intv(idx1, idx2)
}

rle_logi <- function(logi) {

  n <- length(logi)
  y <- logi[-1L] != logi[-n]
  #y <- xor(logi[-1L], logi[-n])
  #y <- (logi[-1L] + logi[-n]) == 1L
  i <- c(which(y), n)

  list(lengths = diff(c(0L, i)),
       values = logi[i])
}

LogiToIntv <- function(logi) {

  r <- rle_logi(logi)
  endPos <- cumsum(r$lengths)
  startPos <- endPos - r$lengths + 1
  #exploit that logi is logical
  startPos <- startPos[r$values]
  endPos <- endPos[r$values]
  length <- r$lengths[r$values]

  win <- rbind(startPos, endPos, length)
  win
}

IntvToLogi <- function(intv, full_length) {

  ret <- vector(length = full_length)
  for (i in seq_len(ncol(intv))) {
    mask <- MaskIntv(intv[, i])
    ret[mask] <- TRUE
  }

  ret
}

FilterMinIntervalSize <- function(interval, min_intv_size) {

  mask <- min_intv_size <= interval[3, ]

  interval[ , mask, drop = FALSE]
}

FilterMaxIntervalSize <- function(interval, max_intv_size) {

  mask <- interval[3, ] <= max_intv_size

  interval[ , mask, drop = FALSE]
}

FilterMinIntervalPos <- function(interval, min_intv_pos) {

  mask <- min_intv_pos <= interval[1, ]

  interval[ , mask, drop = FALSE]
}

FilterMaxIntervalPos <- function(interval, max_intv_pos) {

  mask <- interval[2, ] <= max_intv_pos

  interval[ , mask, drop = FALSE]
}

FirstElement <- function(x, elaborate = NULL) {

  x_name <- deparse(substitute(x))
  if (!length(x)) {
    eval(substitute(err_zero_length(x, elaborate, esc_eval = TRUE)))
  }
  if (length(x) > 1L) {
    msg <- sprintf("%s has length > 1 and only the first element (%s) will be used.",
                   x_name, toString(x[[1]]))
    if (!is.null(elaborate)) {
      msg <- sprintf("%s %s", msg, as.character(elaborate))
    }
    warning(msg)
  }

  x[[1]]
}

ExtractFrom <- function(abf, epoch, episode, channel, lagL = 0L, lagR = lagL) {

  epoch_intv <- GetEpochIntervals(abf)
  intv <- epoch_intv[, episode, epoch]
  if ((lagL + lagR) < intv[3]) {
    intv[1] <- intv[1] + lagL
    intv[2] <- intv[2] - lagR
  } else {
    #This shouldn't really happen
    add_msg <- sprintf("lag too large: L = %d, R = %d", lagL, lagR)
    err_internal_bug(add_msg)
  }
  mask <- MaskIntv(intv)

  abf[mask, episode, channel]
}

ExpandList <- function(x, ref) {

  if (is.list(x)) {
    if (!AssertLength(x, ref)) {
      return(NULL)
    }
  } else {
    x <- rep(list(x), length(ref))
  }

  x
}

abf_list_copy_paste_place_holder <- function(abf) {

  if (IsAbf(abf)) {

  } else if (IsAbfList(abf)) {

  } else {
    err_class_abf_list()
  }

}
