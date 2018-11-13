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

  cvalue <- list(...)
  df <- as.data.frame(sample_index)

  idx <- rep(TRUE, nrow(df))
  for (i in names(cvalue)) {
    if (i == "") {
      nvpair <- cvalue[[i]]
      if (length(nvpair) != 2L) {
        warning(paste0("Unnamed value ", nvpair, " ignored."))
        next
      }
      idx <- idx & (unlist(df[nvpair[1]]) %in% nvpair[2])
    } else {
      idx <- idx & (unlist(df[i]) %in% cvalue[[i]])
    }
  }

  return(sample_index[idx, ])
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

  cvalue <- list(...)
  df <- as.data.frame(sample_index)

  idx <- rep(TRUE, nrow(df))
  for (i in names(cvalue)) {
    if (i == "") {
      nvpair <- cvalue[[i]]
      if (length(nvpair) != 2L) {
        warning(paste0("Unnamed value ", nvpair, " ignored."))
        next
      }
      idx <- idx & !(unlist(df[nvpair[1]]) %in% nvpair[2])
    } else {
      idx <- idx & !(unlist(df[i]) %in% cvalue[[i]])
    }
  }

  return(sample_index[idx, ])
}
