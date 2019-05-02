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

FirstElement <- function(x, message = NULL) {

  x_name <- deparse(substitute(x))
  if (!length(x)) {
    eval(substitute(err_zero_length(x, message, esc_eval = TRUE)))
  }
  if (length(x) > 1L) {
    msg <- sprintf("%s has length > 1 and only the first element (%s) will be used.",
                   x_name, toString(x[[1]]))
    if (!is.null(message)) {
      msg <- sprintf("%s %s", msg, as.character(message))
    }
    warning(msg)
  }

  x[[1]]
}

ExpandList <- function(x, ref) {

  MatchList(x, len = length(ref))
}

MatchList <- function(x, len) {

  if (is.list(x)) {
    rep(x, length.out = len)
  } else {
    rep(list(x), length.out = len)
  }
}

ExpandIntvList <- function(abf, intv) {
  if (IsAbf(abf)) {
    if (is.null(intv)) {
      Intv(startPos = 1L, endPos = nPts(abf))
    } else {
      intv
    }
  } else {
    if (is.null(intv)) {
      lapply(abf, function(x) Intv(startPos = 1L, endPos = nPts(x)))
    } else {
      ExpandList(intv, ref = abf)
    }
  }
}
