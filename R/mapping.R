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

  return(packed)
}

#' Mapping function to an nd-array along specific axis.
#'
#' @param x an nd-array.
#' @param f a function to map.
#' @param along the axis to map the function to.
#' @param pack_args whether to pack arguments for f.
#' @param ... other argumetns passed to f.
#'
#' @return an array (dimension depending on returned dimension of f)
#' @export
#'
mapnd <- function(x, f, along = 1L, pack_args = FALSE, ...) {

  ndim <- seq_len(length(dim(x)))
  margin <- ndim[-along]

  if (pack_args) {
    packed <- PackArgs(f, ...)
    ret <- apply(x, margin, packed)
  } else {
    ret <- apply(x, margin, f, ...)
  }

  return(ret)
}

#' Wrap a mapping function to batch process abf channel data.
#'
#' The returned function accepts an abf object and maps its channel data in the
#' given intv to the mapping function. If intv is not given, it maps the whole
#' channel to mapping function instead. The return value type is depended on
#' whether abf_id_func and epi_id_func are present. If so, a data.frame is returned,
#' otherwise a matrix is returned. The column names are determined by chan_id_func,
#' and if chan_id_func is missing, a column name of numeric channel id will be
#' used.
#'
#' @param map_func a mapping function to process abf channel data, such as mean, sum etc.
#' @param abf_id_func OPTIONAL, a function accepts an abf object and returns an identifier of the objects.
#' @param epi_id_func OPTIONAL, a function accepts an abf object and returns a vector of identifiers of all episodes.
#' @param chan_id_func OPTIONAL, a function accepts an abf object and returns a vectors of identifiers of all channels.
#' @param channel OPTIONAL, a channel/channels to process.
#' @param ... further arguments passed to map_func
#'
#' @return a function of f(abf, intv).
#' @export
#'
WrapMappingFunc <- function(map_func, abf_id_func = NULL, epi_id_func = NULL,
                            chan_id_func = DefaultChanLabel, channel, ...) {

  if (missing(channel)) {
    channel <- NULL
  }

  f <- function(abf, intv = NULL) {

    if (!IsAbf(abf)) {
      err_class_abf()
    }
    if (is.null(channel)) {
      channel <- GetAllChannels(abf)
    }
    if (!AssertChannel(abf, channel)) {
      err_channel()
    }

    #dims
    nch <- length(channel)
    nep <- nEpi(abf)
    ret <- matrix(NA, nrow = nep, ncol = nch)
    if (is.null(intv)) {
      mask <- seq_len(nPts(abf))
    } else {
      mask <- MaskIntv(intv)
    }

    #map
    ret <- mapnd(x = abf[mask, ,channel], f = map_func, along = 1L,
                 pack_args = FALSE, ...)

    #additioinal cols and colnames
    ids <- NULL
    idnames <- NULL
    if (!is.null(abf_id_func)) {
      ids <- abf_id_func(abf)
      idnames <- "id"
    }
    if (!is.null(epi_id_func)) {
      ids <- cbind(ids, epi_id_func(abf)[GetAvailEpisodes(abf)])
      idnames <- c(idnames, "Episode")
    }
    if (is.null(chan_id_func)) {
      vnames <- as.character(channel)
    } else {
      vnames <- as.character(chan_id_func(abf)[channel])
    }
    if (is.null(ids)) {
      #no named id columns attached
      colnames(ret) <- vnames
    } else {
      #convert to data.frame before cbind, since matrix can't hold multiple data types.
      ret <- cbind(ids, as.data.frame(ret))
      colnames(ret) <- c(idnames, vnames)
    }

    return(ret)
  }

  return(f)
}

#' Convenient alias of WrapMappingFunc()
#'
#' @param ... See WrapMappingFunc for more details.
#'
#' @return a function of f(abf, intv)
#' @export
#'
wrap <- function(...) {

  WrapMappingFunc(...)
}

