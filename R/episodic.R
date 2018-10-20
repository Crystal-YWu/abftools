#' EpisodicIntervalMeans calculates interval means of a list of abf objects
#'
#' @param abf_list
#' @param intv_list
#' @param channel
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
EpisodicIntervalMeans <- function(abf_list, intv_list, channel = 1, na.rm = TRUE) {

  f <- function(x) colMeans(x, na.rm)

  #na.rm is problematic here since na.rm make colX functions return NaN for removed
  #episodes, so we replace those with NA

  mx <- EpisodicInterval_f(abf_list, intv_list, channel, f)
  if (na.rm)
    mx[is.nan(mx)] <- NA

  return(mx)
}

#' EpisodicIntervalSds calculates interval standard deviations of a list of abf objects
#'
#' @param abf_list
#' @param intv_list
#' @param channel
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
EpisodicIntervalSds <- function(abf_list, intv_list, channel = 1, na.rm = TRUE) {

  f <- function(x) colSds(x, na.rm)

  #na.rm is problematic here since na.rm make colX functions return NaN for removed
  #episodes, so we replace those with NA
  mx <- EpisodicInterval_f(abf_list, intv_list, channel, f)
  if (na.rm)
    mx[is.nan(mx)] <- NA

  return(mx)
}

#' EpisodicIntervalSems calculates interval SEM of a list of abf objects
#'
#' @param abf_list
#' @param intv_list
#' @param channel
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
EpisodicIntervalSems <- function(abf_list, intv_list, channel = 1, na.rm = TRUE) {

  f <- function(x) colSems(x, na.rm)

  #na.rm is problematic here since na.rm make colX functions return NaN for removed
  #episodes, so we replace those with NA
  mx <- EpisodicInterval_f(abf_list, intv_list, channel, f)
  if (na.rm)
    mx[is.nan(mx)] <- NA

  return(mx)
}
