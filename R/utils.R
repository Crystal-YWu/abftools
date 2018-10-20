#' Title
#'
#' @param df
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
colSds <- function(df, na.rm = FALSE) {

  n <- ifelse(na.rm, colSums(!is.na(df)), nrow(df))
  #implementation for the missing colSds, may not be most effective
  ret <- colMeans(df * df, na.rm) - colMeans(df, na.rm)^2
  ret <- sqrt(ret * n / (n - 1))

  return(ret)
}

#' Title
#'
#' @param df
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
colSems <- function(df, na.rm = FALSE) {

  sds <- colSds(df, na.rm)
  sqn <- sqrt(length(sds))

  return(sds / sqn)
}


#' Title
#'
#' @param intv
#' @param startPos
#' @param endPos
#'
#' @return
#' @export
#'
#' @examples
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
