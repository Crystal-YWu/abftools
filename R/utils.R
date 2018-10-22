#' Calculate column standard deviations
#'
#' @param df a data frame or data frame like 2d data
#' @param na.rm remove NAs
#'
#' @return calculated column sd
#' @export
#'
colSds <- function(df, na.rm = FALSE) {

  n <- ifelse(na.rm, colSums(!is.na(df)), nrow(df))
  #implementation for the missing colSds, may not be most effective
  ret <- colMeans(df * df, na.rm) - colMeans(df, na.rm)^2
  ret <- sqrt(ret * n / (n - 1))

  return(ret)
}

#' Calculate column standard error of measurements
#'
#' @param df a data frame of data frame like 2d data
#' @param na.rm remove NAs
#'
#' @return calculated column sem
#' @export
#'
colSems <- function(df, na.rm = FALSE) {

  sds <- colSds(df, na.rm)
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
#' @examples
#' #intv itself is changed, no need to assign
#' SetIntv(intv, 100L, 500L)
SetIntv <- function(intv, startPos, endPos) {

  if (startPos >= endPos) {
    err_intv_pos("SetIntv")
  }
  if (abs(endPos - startPos) < 3L) {
    warning("SetIntv: interval size smaller than 3.")
  }

  eval.parent(substitute({
    intv <- c(startPos, endPos, endPos - startPos + 1L)
  }))
}
