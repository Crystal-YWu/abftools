#' Select samples conditionally
#'
#' This function provides a naïve yet simple way to select desired samples provided
#' in sample_index. A sample_index should have named columns which descpribes
#' properties of your samples, any unnamed columns or conditions will be ignored.
#' Be noticed that all names are case-sensitive.
#'
#' @param sample_index a data frame like object with descriptions of samples
#' @param ... a list of conditions composed of name = value pairs.
#'
#' @return selected rows of sample_index
#' @export
#'
SelectSample <- function(sample_index, ...) {

  args <- list(...)
  idx <- rep(TRUE, nrow(sample_index))

  keys <- names(args)
  cols <- names(sample_index)
  for (i in seq_along(args)) {
    if (keys[i] == "") {
      if (length(args[i]) != 2L) {
        warning(paste("Unnamed value", args[i], "ignored."))
        next
      }
      k <- args[[i]][1]
      v <- args[[i]][2]
    } else {
      k <- keys[i]
      v <- args[[i]]
    }
    if (!k %in% cols) {
      next
    }
    idx <- idx & (sample_index[[k]] %in% v)
  }

  sample_index[idx, ]
}

#' Exclude samples conditionally
#'
#' This function provides a naïve yet simple way to exclude unwanted samples
#' provided in sample_index. A sample_index should have named columns which
#' descpribe properties of your samples, any unnamed columns or conditions will
#' be ignored. Be noticed that all names are case-sensitive.
#'
#' @param sample_index a data frame like object with descriptions of samples
#' @param ... a list of conditions composed of name = value pairs.
#'
#' @return remaining rows of sample_index
#' @export
#'
ExcludeSample <- function(sample_index, ...) {

  args <- list(...)
  idx <- rep(TRUE, nrow(sample_index))

  keys <- names(args)
  cols <- names(sample_index)
  for (i in seq_along(args)) {
    if (keys[i] == "") {
      if (length(args[i]) != 2L) {
        warning(paste("Unnamed value", args[i], "ignored."))
        next
      }
      k <- args[[i]][1]
      v <- args[[i]][2]
    } else {
      k <- keys[i]
      v <- args[[i]]
    }
    if (!k %in% cols) {
      next
    }
    idx <- idx & !(sample_index[[k]] %in% v)
  }

  sample_index[idx, ]
}
