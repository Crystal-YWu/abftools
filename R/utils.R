#' Calculate column/row standard error of measurements
#'
#' @param x an 2d numeric
#' @param na.rm remove NAs
#' @param ... passed to colSds/rowSds()
#'
#' @return calculated column sem
#' @export
#'
colSems <- function(x, na.rm = FALSE, ...) {

  sds <- matrixStats::colSds(x, na.rm = na.rm, ...)
  if (na.rm) {
    na <- is.na(x)
    sqn <- sqrt(matrixStats::colSums2(!na))
  } else {
    sqn <- sqrt(nrow(x))
  }

  sds / sqn
}

#' @rdname colSems
#' @export
#'
rowSems <- function(x, na.rm = FALSE, ...) {

  sds <- matrixStats::rowSds(x, na.rm = na.rm, ...)
  if (na.rm) {
    na <- is.na(x)
    sqn <- sqrt(matrixStats::rowSums2(!na))
  } else {
    sqn <- sqrt(ncol(x))
  }

  sds / sqn
}

#' Generate an interval.
#'
#' Only two or the arguments are needed. If all are given, len will be ignored.
#'
#' @param startPos OPTIONAL, start position.
#' @param endPos OPTIONAL, end position.
#' @param len OPTIONAL, length of the interval.
#'
#' @return an interval.
#' @export
#'
Intv <- function(startPos, endPos, len) {

  if (missing(startPos)) {
    startPos <- endPos - len + 1L
  }
  if (missing(endPos)) {
    endPos <- startPos + len - 1L
  }
  if (startPos > endPos) {
    err_intv_pos()
  }
  len <- endPos - startPos + 1L

  intv <- c(startPos, endPos, len)
  names(intv) <- c("startPos", "endPos", "length")

  intv
}

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

#' Set intervals, by-ref behaviour
#'
#' @param intv an interval.
#' @param startPos starting position of the new interval.
#' @param endPos ending position of the new interval.
#'
#' @return an interval.
#' @export
#'
SetIntv <- function(intv, startPos, endPos) {

  if (startPos >= endPos) {
    err_intv_pos()
  }
  if (abs(endPos - startPos) < 3L) {
    warning("SetIntv: interval size smaller than 3.")
  }

  eval.parent(substitute({
    intv <- Intv(startPos, endPos)
  }))
}
