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
#' @examples
#'
#' records <- read.csv("experiment_record.csv")
#' selected_oocyte <- SelectSample(records, gene = c("B", "N1"), type = "gapfree")
#' #you can also select empty elements by using NA
#' selected_oocyte <- SelectSample(records, OocyteId = 5, remakes = NA)
#' #for columns with special names, use a back quote
#' selected_oocyte <- SelectSample(records, `funny behave ?~` = c(NA, "no"))
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
      idx <- idx & (df[nvpair[1]] %in% nvpair[2])
    } else {
      idx <- idx & (df[i] %in% cvalue[[i]])
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
#' @examples
#'
#' records <- read.csv("experiment_record.csv")
#' selected_oocyte <- ExcludeSample(records, gene = c("B", "N1"), bad = "yes")
#' #you can also select empty elements by using NA
#' selected_oocyte <- ExcludeSample(records, OocyteId = c(NA, 3))
#' #for columns with special names, use a back quote
#' selected_oocyte <- ExcludeSample(records, `funny behave ?~` = "Yes")
ExcludeSample <- function(sample_index, ...) {

  cvalue <- list(...)
  df <- as.data.frame(sample_index)

  idx <- rep(FALSE, nrow(df))
  for (i in names(cvalue)) {
    if (i == "") {
      nvpair <- cvalue[[i]]
      if (length(nvpair) != 2L) {
        warning(paste0("Unnamed value ", nvpair, " ignored."))
        next
      }
      idx <- idx | (df[nvpair[1]] %in% nvpair[2])
    } else {
      idx <- idx | (df[i] %in% cvalue[[i]])
    }
  }

  return(sample_index[idx, ])
}
