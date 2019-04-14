#TODO: Implement a by-ref apply_unsafe() and apply_colFunc_unsafe() in C should
#further improve performance.

#' An unsafe apply()
#'
#' Basically apply() without validating anything.
#'
#' @param x an array.
#' @param along dimension to apply along.
#' @param func_ a function to apply.
#' @param keep_dim keep dim order
#' @param ... passed to func_
#'
#' @return an array.
#'
apply_unsafe <- function(x, along, func_, keep_dim = FALSE, ...) {

  func_ <- match.fun(func_)

  d <- dim(x)
  dim_len <- length(d)
  dim_perm <- seq_len(dim_len)
  if (!dim_len) {
    stop("x must be multi-dimensional.")
  }

  data_dim <- along

  dim_data <- d[data_dim]
  dim_loop <- d[-data_dim]
  len_loop <- prod(dim_loop)
  loop_dim <- dim_perm[-along]

  x <- aperm(x, c(data_dim, loop_dim))
  dim(x) <- c(dim_data, len_loop)

  ans <- vector(mode = "list", length = len_loop)
  for (i in seq_len(len_loop)) {
    ans[[i]] <- forceAndCall(1, func_, x[, i], ...)
  }

  #lengths are not checked, should throw error in dim<-() if anything wrong
  l_ans <- length(ans[[1]])
  ans <- unlist(ans)

  if (l_ans == 1L) {
    #collapse dim
    if (keep_dim) {
      dim(ans) <- c(1L, dim_loop)
    } else {
      dim(ans) <- dim_loop
    }
  } else {
    #attach dim
    dim(ans) <- c(length(ans) %/% len_loop, dim_loop)
  }

  if (keep_dim) {
    aperm(ans, order(c(data_dim, loop_dim)))
  } else {
    ans
  }
}

#' An unsafe apply() that applies a column function
#'
#' Same as apply_unsafe() but applies a column function. The same funtionality
#' can be achieved by aperm(...) %>% apply(...), however, apply_colFunc_unsafe()
#' permutates and set dim only once for data to reduce memory footprint.
#'
#' @param x an array.
#' @param along dimension to apply along.
#' @param colFunc_ a column function to apply.
#' @param keep_dim keep dim order
#' @param ... passed to colFunc_
#'
#' @return an array
#'
apply_colFunc_unsafe <- function(x, along, colFunc_, keep_dim = FALSE, ...) {

  colFunc_ <- match.fun(colFunc_)

  d <- dim(x)
  dim_len <- length(d)
  dim_perm <- seq_len(dim_len)
  if (dim_len < 3L) {
    stop("x must be at least 3-dimensional.")
  }

  data_dim <- along
  max_dim <- which.max(d)

  if (along == max_dim) {
    #along is already largest dim, set to 1st dim of remaining dimensions to
    #reduce memory footprint.
    col_dim <- dim_perm[-data_dim][1L]
  } else {
    col_dim <- max_dim
  }

  dim_data <- d[data_dim]
  dim_col <- d[col_dim]
  dim_loop <- d[-c(data_dim, col_dim)]
  len_loop <- prod(dim_loop)
  loop_dim <- dim_perm[-c(data_dim, col_dim)]

  x <- aperm(x, c(data_dim, col_dim, loop_dim))
  dim(x) <- c(dim_data, dim_col, len_loop)

  ans <- vector(mode = "list", length = len_loop)
  for (i in seq_len(len_loop)) {
    ans[[i]] <- forceAndCall(1, colFunc_, x[,, i], ...)
  }

  l_ans <- length(ans[[1]])
  ans <- unlist(ans)

  if (l_ans == dim_col) {
    #collapse dim
    if (keep_dim) {
      dim(ans) <- c(1L, dim_col, dim_loop)
    } else {
      dim(ans) <- c(dim_col, dim_loop)
    }
  } else {
    #I can't think of any colFunc would return multiple values for each cols,
    #but anyway, attach dim
    dim(ans) <- c(length(ans) %/% prod(c(dim_col, dim_loop)), dim_col, dim_loop)
  }

  if (keep_dim) {
    aperm(ans, order(c(data_dim, col_dim, loop_dim)))
  } else {
    ans
  }
}

#' Packing arguments in function call
#'
#' This is a helper function that can be handy when mapping function calls to
#' multiple data. It wraps a function f, which accepts multiple arguments, into
#' a new function that accept a vector as its argument.
#'
#' @param f a function
#' @param ... other arguments passed to f
#'
#' @return a function that accepts a vector argument
#' @export
#'
#' @examples
#' ivpair <- c(10, 50)
#' resistance <- function(v, i) v / i
#' f <- PackArgs(resistance)
#' r1 <- resistance(ivpair[1], ivpair[2])
#' r2 <- f(ivpair)
#'
PackArgs <- function(f, ...) {

  dots <- list(...)
  packed <- function(vec) {
    do.call(f, c(as.list(vec), dots))
  }

  packed
}

#' Mapping function to an nd-array along a specific axis.
#'
#' mapnd() applies a function along a specific axis/dimension of x. However,
#' if a corresponding column function is available, using mapnd_col() can be
#' more efficient e.g. use mapnd_col(x, colMeans) instead of mapnd(x, mean).
#' For convenience, you can also call mapnd() by supplying a function name, e.g.
#' mapnd(x, "mean"). In this case, mapnd will first lookup any corresponding
#' column vectorised function by calling find_colFunc() and if a colFunc is found,
#' mapnd_col will be used instead for better performance.
#'
#' @param x an nd-array.
#' @param func a function to map.
#' @param colFunc a column function.
#' @param func_name a optimised function name.
#' @param along the axis to map the function to (dims to collapse).
#' @param pack_args whether to pack arguments for func.
#' @param ... other arguments passed to func.
#'
#' @return an array (dimension depending on returned dimension of func)
#' @export
#'
mapnd <- function(x, func, along = 1L, pack_args = FALSE, ...) {

  if (length(along) > 1L) {
    err_invalid_axis(along)
  }

  if (pack_args) {
    packed <- PackArgs(func, ...)
    apply_unsafe(x = x, along = along, func_ = packed)
  } else {
    if (is.character(func)) {
      guess_func <- find_colFunc(func_name = func)
      if (!is.null(guess_func)) {
        return(
          apply_colFunc_unsafe(x = x, along = along, colFunc_ = guess_func, ...)
        )
      }
    }
    apply_unsafe(x = x, along = along, func_ = func, ...)
  }
}

#' @rdname mapnd
#' @export
#'
mapnd_col <- function(x, colFunc, along = 1L, ...) {

  if (length(along) > 1L) {
    err_invalid_axis(along)
  }

  apply_colFunc_unsafe(x = x, along = along, colFunc_ = colFunc, ...)
}

#' Sample an nd-array evenly by ratio.
#'
#' Like mapnd() and mapnd_col(), for performance consideration, samplend_col is
#' strongly encouraged is a sampling function is used.
#'
#' @param x an nd-array.
#' @param ratio a sampling ratio.
#' @param func a sampling function.
#' @param colFunc a column function.
#' @param func_name a optimised function name.
#' @param along the axis/dimension to sample along.
#' @param ... passed to func/colFunc.
#'
#' @return a sampled nd-array.
#' @export
#'
samplend <- function(x, ratio = 1L, func = NULL, along = 1L, ...) {

  if (length(along) > 1L) {
    err_invalid_axis(along)
  }

  if (is.character(func)) {
    guess_func <- find_colFunc(func_name = func)
    if (!is.null(guess_func)) {
      return(
        apply_unsafe(x = x, along = along, func_ = sample1d_col,
                     ratio = ratio, colFunc = guess_func, ...)
      )
    }
  }

  apply_unsafe(x = x, along = along, func_ = sample1d, keep_dim = TRUE,
               ratio = ratio, func = func, ...)
}

#' @rdname samplend
#' @export
#'
samplend_col <- function(x, ratio = 1L, colFunc = NULL, along = 1L, ...) {

  if (length(along) > 1L) {
    err_invalid_axis(along)
  }

  apply_unsafe(x = x, along = along, func_ = sample1d_col, keep_dim = TRUE,
               ratio = ratio, colFunc = colFunc, ...)
}

sample1d <- function(x, ratio, func = NULL, ...) {

  if (ratio == 1L) {
    return(x)
  }

  n <- length(x)
  idx <- seq.int(from = 1L, to = n, by = ratio)
  nidx <- length(idx)

  if (is.null(func)) {
    return(x[idx])
  }

  idx2 <- c(idx[2:nidx] - 1L, n)
  ans <- matrixStats::allocVector(length = nidx, value = 0)
  for (i in seq_len(nidx)) {
    ans[i] <- func(x[seq.int(from = idx[i], to = idx2[i])], ...)
  }

  ans
}

sample1d_rolled <- function(x, ratio, func = NULL, ...) {

  ans <- sample1d(x = x, ratio = ratio, func = func, ...)
  ans <- matrix(ans, nrow = ratio, ncol = length(ans), byrow = TRUE)
  dim(ans) <- NULL
  length(ans) <- length(x)

  ans
}

sample1d_col <- function(x, ratio, colFunc = NULL, ...) {

  if (ratio == 1L) {
    return(x)
  }

  n <- length(x)
  idx <- seq.int(from = 1L, to = n, by = ratio)
  nidx <- length(idx)

  if (is.null(colFunc)) {
    return(x[idx])
  }

  if (n %% ratio == 0) {
    dim(x) <- c(ratio, n %/% ratio)
    data <- colFunc(x, ...)
  } else {
    #div
    block_idx <- idx[nidx] - 1L
    block_data <- x[seq_len(block_idx)]
    dim(block_data) <- c(ratio, block_idx %/% ratio)
    block_sv <- colFunc(block_data, ...)
    #rem
    block_data <- x[seq.int(idx[nidx], n)]
    dim(block_data) <- c(length(block_data), 1L)
    last_sv <- colFunc(block_data, ...)
    data <- c(block_sv, last_sv)
  }

  data
}

sample1d_col_rolled <- function(x, ratio, colFunc = NULL, ...) {

  ans <- sample1d_col(x = x, ratio = ratio, colFunc = colFunc, ...)
  ans <- matrix(ans, nrow = ratio, ncol = length(ans), byrow = TRUE)
  dim(ans) <- NULL
  length(ans) <- length(x)

  ans
}

sample3d_dim1_col <- function(x, ratio, colFunc = NULL, ...) {

  d <- dim(x)
  nd1 <- d[1]
  nd2 <- d[2]
  nd3 <- d[3]

  if (ratio == 1L) {
    return(x)
  }

  idx_smpl <- seq.int(from = 1L, to = nd1, by = ratio)
  nsmpl <- length(idx_smpl)
  ans <- array(x[idx_smpl,,], dim = c(nsmpl, nd2, nd3))

  if (!is.null(colFunc)) {
    if (nd1 %% ratio == 0L) {
      ncol <- nd1 %/% ratio
      for (id3 in seq_len(nd3)) {
        for (id2 in seq_len(nd2)) {
          blk <- x[, id2, id3]
          dim(blk) <- c(ratio, ncol)
          blk_fv <- colFunc(blk, ...)
          ans[, id2, id3] <- blk_fv
        }
      }
    } else {
      div <- idx_smpl[nsmpl] - 1L
      div_idx <- seq_len(div)
      div_ncol <- div %/% ratio
      rem_idx <- seq.int(from = idx_smpl[nsmpl], to = nd1)
      for (id3 in seq_len(nd3)) {
        for (id2 in seq_len(nd2)) {
          #div
          blk_div <- x[div_idx, id2, id3]
          dim(blk_div) <- c(ratio, div_ncol)
          blk_fv <- colFunc(blk_div)
          #rem
          blk_rem <- x[rem_idx, id2, id3]
          dim(blk_rem) <- c(length(blk_rem), 1L)
          last_fv <- colFunc(blk_rem)
          #d1
          ans[, id2, id3] <- c(blk_fv, last_fv)
        }
      }
    }
  }

  ans
}

#' Match function name to a corresponding column functions known to abftools.
#'
#' @param func_name a charater vector, function name.
#'
#' @return a function, or NULL if no column function was found.
#' @export
#'
#' @examples
#' f <- find_colFunc("mean") #matrixStats::colMeans2
#' f <- find_colFunc("cos") #NULL, since no column vectorised cos is known to abftools.
find_colFunc <- function(func_name = c("all", "any", "anyNA", "count", "cummax",
                                       "cummin", "cumprod", "cumsum", "diff",
                                       "iqrDiff", "iqr", "logSumExp", "madDiff",
                                       "mad", "max", "mean", "median", "min",
                                       "orderStats", "prod", "quantile", "range",
                                       "rank", "sdDiff", "sd", "sum", "tabulate",
                                       "varDiff", "var", "weightedMad", "weightedMean",
                                       "weightedMedian", "weightedSd", "weightedVar",
                                       "sem")) {

  func_name <- tryCatch(match.arg(func_name),
                        error = function(e) {
                          func_name
                        })

  colFunc <- switch(func_name,
                    #provided by matrixStats
                    all = matrixStats::colAlls,
                    any = matrixStats::colAnys,
                    anyNA = matrixStats::colAnyNAs,
                    count = matrixStats::colCounts,
                    cummax = matrixStats::colCummaxs,
                    cummin = matrixStats::colCummins,
                    cumprod = matrixStats::colCumprods,
                    cumsum = matrixStats::colCumsums,
                    diff = matrixStats::colDiffs,
                    iqrDiff = matrixStats::colIQRDiffs,
                    iqr = matrixStats::colIQRs,
                    logSumExp = matrixStats::colLogSumExps,
                    madDiff = matrixStats::colMadDiffs,
                    mad = matrixStats::colMads,
                    max = matrixStats::colMaxs,
                    mean = matrixStats::colMeans2,
                    median = matrixStats::colMedians,
                    min = matrixStats::colMins,
                    orderStats = matrixStats::colOrderStats,
                    prod = matrixStats::colProds,
                    quantile = matrixStats::colQuantiles,
                    range = matrixStats::colRanges,
                    rank = matrixStats::colRanks,
                    sdDiff = matrixStats::colSdDiffs,
                    sd = matrixStats::colSds,
                    sum = matrixStats::colSums2,
                    tabulate = matrixStats::colTabulates,
                    varDiff = matrixStats::colVarDiffs,
                    var = matrixStats::colVars,
                    weightedMad = matrixStats::colWeightedMads,
                    weightedMean = matrixStats::colWeightedMeans,
                    weightedMedian = matrixStats::colWeightedMedians,
                    weightedSd = matrixStats::colWeightedSds,
                    weightedVar = matrixStats::colWeightedVars,
                    #abftools implemented
                    sem = colSems,
                    NULL)

  colFunc
}
