#' Calculate column standard error of measurements
#'
#' @param df a data frame of data frame like 2d data
#' @param na.rm remove NAs
#'
#' @return calculated column sem
#' @export
#'
colSems <- function(df, na.rm = FALSE) {

  sds <- matrixStats::colSds(df, na.rm)
  sqn <- sqrt(length(sds))

  return(sds / sqn)
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
    intv <- c(startPos, endPos, endPos - startPos + 1L)
  }))
}

push <- function(x, items) {

  if (is.vector(x) || is.list(x)) {
    eval.parent(substitute({
      x <- append(x, items)
    }))
  } else {
    err_class_vec_list()
  }
}

pop <- function(x) {

  #this contaminates parent's env, need better solution
  if (is.vector(x)) {
    eval.parent(substitute({
      item <- x[length(x)]
      x <- x[-length(x)]
      item
    }))
  } else if (is.list(x)) {
    eval.parent(substitute({
      item <- x[[length(x)]]
      x <- x[[-length(x)]]
      item
    }))
  } else {
    err_class_vec_list()
  }
}
