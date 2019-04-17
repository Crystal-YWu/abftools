#' Reduction of x along a specific dimension
#'
#' mapnd() and reduce_dim() is not true map-reduce since mapnd() does not emit
#' anything. However, these functions are designed to mimic the map-reduce paradigm
#' in hope of facilitating a uniform data processing scheme for all objects.
#'
#' reduce_lastdim() reduces x along last dimension, and it is a bit more efficient
#' since no permutation is done to x.
#'
#' @param x data to reduce.
#' @param along axis/dimension to reduce.
#' @param reduce_func a function to perform the reduction.
#' @param names set names to results.
#' @param ... passed to reduce_func.
#'
#' @return a list
#' @export
#'
#' @examples
#' \dontrun{
#' max_avg_current <- function(abf) {
#'   abf %>%
#'      samplend(ratio = 1000, func = "mean") %>%
#'      reduce_lastdim(reduce_func = max)
#' }
#'
#' plot_iv <- function(abf, intv) {
#'   p <- abf %>%
#'           extract(MaskIntv(intv),,) %>%
#'           mapnd(func = "mean") %>%
#'           reduce_lastdim(names = c("current", "voltage")) %>%
#'           data.frame() %>%
#'           ggplot(mapping = aes_string("current", "voltage"))
#'   p + geom_line()
#' }
#' }
reduce_along <- function(x, along, reduce_func = NULL, names = NULL, ...) {

  dx <- dim(x)
  ndim <- length(dx)
  dim_perm <- seq_len(ndim)
  dim_prev <- dim_perm[-along]
  dim_last <- along

  x <- aperm(x, perm = c(dim_prev, dim_last))
  dim(x) <- c(prod(dx[dim_prev]), dx[dim_last])

  ans <- vector(mode = "list", length = dx[dim_last])
  for (i in seq_len(dx[dim_last])) {
    ans[[i]] <- x[, i]
  }
  if (!is.null(names)) {
    base::names(ans) <- as.character(names)
  }

  if (!is.null(reduce_func)) {
    lapply(ans, reduce_func, ...)
  } else {
    ans
  }
}

#' @rdname reduce_along
#' @export
#'
reduce_lastdim <- function(x, reduce_func = NULL, names = NULL, ...) {

  dx <- dim(x)
  ndim <- length(dx)
  d_prev <- dx[seq_len(ndim - 1L)]
  d_last <- dx[ndim]

  dim(x) <- c(prod(d_prev), d_last)

  ans <- vector(mode = "list", length = d_last)
  for (i in seq_len(d_last)) {
    ans[[i]] <- x[, i]
  }
  if (!is.null(names)) {
    base::names(ans) <- as.character(names)
  }

  if (!is.null(reduce_func)) {
    lapply(ans, reduce_func, ...)
  } else {
    ans
  }
}

