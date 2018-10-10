UniformYLim <- function(abf_list, chan_id, intv_list = NULL) {

  ret <- c(0, 0)
  for (i in seq_along(abf_list)) {
    if (is.null(intv_list)) {
      focus <- nrow(abf_list[[i]][[chan_id]]) %/% 2
    } else {
      if (is.na(intv_list[[i]][1]))
        focus <- nrow(abf_list[[i]][[chan_id]]) %/% 2
      else
        focus <- (intv_list[[i]][1] + intv_list[[i]][2]) %/% 2
    }
    val <- sort(as.vector(abf_list[[i]][focus, , chan_id]))
    lower <- val[1] - abs(val[1] - val[2]) * 0.5
    n <- length(val)
    upper <- val[n] + abs(val[n] - val[n - 1]) * 0.5
    ret <- range(ret, lower, upper)
  }

  return(ret)
}