#' Calculate column standard error of measurements
#'
#' @param df a data frame of data frame like 2d data
#' @param na.rm remove NAs
#'
#' @return calculated column sem
#' @export
#'
colSems <- function(df, na.rm = FALSE) {

  sds <- matrixStats::colSds(df, na.rm = na.rm)
  sqn <- sqrt(length(sds))

  sds / sqn
}

#' Generate an interval.
#'
#' Only two or the arguments are needed. If all are given, len will be ignored.
#'
#' @param startPos start position.
#' @param endPos end position.
#' @param len length of the interval.
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
  if (startPos >= endPos) {
    err_intv_pos()
  }
  len <- endPos - startPos + 1L

  intv <- c(startPos, endPos, len)
  names(intv) <- c("startPos", "endPos", "length")

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

#' Apply a function over a list of abf objects.
#'
#' @param abf_list a list of abf objects.
#' @param FUN the function to be applied to each episode of the abf objects.
#' @param ... further arguments passed to FUN.
#' @param channel channel of the abf objects to apply.
#' @param intv OPTIONAL, the interval to apply FUN.
#' @param gen_names whether to generate row names.
#'
#' @return a matrix, of which each row represents an episode and column represents an abf object in the list.
#' @export
#'
abfapply <- function(abf_list, FUN, ..., channel = 1L, intv = NULL, gen_names = FALSE) {

  if (!IsAbfList(abf_list)) {
    err_class_abf_list()
  }
  channel <- FirstElement(unlist(channel))
  for (tmp in abf_list) {
    if (!AssertChannel(tmp ,channel)) {
      err_channel()
    }
  }

  intv <- ExpandList(intv, abf_list)
  if (is.null(intv)) {
    err_assert_len(intv, abf_list)
  }

  f <- WrapMappingFunc(FUN, channel = channel, abf_id_func = NULL, epi_id_func = NULL,
                       chan_id_func = NULL, ...)
  ret <- NULL
  for (i in seq_along(abf_list)) {
    ret <- cbind(ret, f(abf_list[[i]], intv[[i]]))
  }
  colnames(ret) <- unlist(GetTitle(abf_list))

  if (gen_names) {
    rownames(ret) <- DefaultEpiLabel(abf_list[[1]])
  }

  ret
}
