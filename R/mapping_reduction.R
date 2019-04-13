reduce_lastdim <- function(x, cols = NULL) {

  dx <- dim(x)
  ndx <- length(dx)
  d_last <- dx[ndx]
  d_prev <- dx[seq_len(ndx - 1L)]

  dim(x) <- c(prod(d_prev), d_last)

  ans <- vector(mode = "list", length = d_last)
  for (i in seq_len(d_last)) {
    ans[[i]] <- x[, i]
  }
  if (!is.null(cols)) {
    names(ans) <- as.character(cols)
  }

  ans
}
