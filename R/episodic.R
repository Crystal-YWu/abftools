#' EpisodicIntervalMeans calculates interval means of a list of abf objects
#'
#' @param abf_list a list of abf objects.
#' @param intv_list a list of intervals.
#' @param channel channel to calculated, channel id is 1-based.
#' @param na.rm remove NAs.
#'
#' @return a vector of calculated values.
#' @export
#'
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
#' @param abf_list a list of abf objects.
#' @param intv_list a list of intervals.
#' @param channel channel to calculated, channel id is 1-based.
#' @param na.rm remove NAs.
#'
#' @return a vector of calculated values.
#' @export
#'
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
#' @param abf_list a list of abf objects.
#' @param intv_list a list of intervals.
#' @param channel channel to calculated, channel id is 1-based.
#' @param na.rm remove NAs.
#'
#' @return a vector of calculated values.
#' @export
#'
EpisodicIntervalSems <- function(abf_list, intv_list, channel = 1, na.rm = TRUE) {

  f <- function(x) colSems(x, na.rm)

  #na.rm is problematic here since na.rm make colX functions return NaN for removed
  #episodes, so we replace those with NA
  mx <- EpisodicInterval_f(abf_list, intv_list, channel, f)
  if (na.rm)
    mx[is.nan(mx)] <- NA

  return(mx)
}
