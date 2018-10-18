#' Title
#'
#' @param data_index
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
SelectSample <- function(data_index, ...) {

  cvalue <- list(...)

  idx <- rep(TRUE, nrow(data_index))
  for (i in names(cvalue)) {
    idx <- idx & (unlist(data_index[i]) %in% cvalue[[i]])
  }

  return(data_index[idx, ])
}