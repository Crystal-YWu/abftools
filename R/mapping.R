unpack_arg <- function(f, ...) {

  dots <- list(...)
  unpacked <- function(vec) {
    do.call(f, c(as.list(vec), dots))
  }

  return(unpacked)
}

abfmap <- function(abf, f, along = 1L, intv = NULL, unpack_args = FALSE,
                   drop_dim = FALSE) {


}

map3d <- function(x, f, along = 1L) {

  perm_in <- switch(along,
                    c(1, 2, 3), #do not perm
                    c(2, 1, 3),
                    c(3, 1, 2))
  perm_out <- switch(along,
                     c(1, 2, 3), #do not perm
                     c(2, 1, 3),
                     c(3, 1, 2))

  if (along == 1L) {
    tmp <- x
  } else {
    tmp <- aperm(x, perm_in)
  }
  test <- f(tmp[, 1L, 1L])
  n <- length(test)

  itrs <- dim(tmp)
  ret <- array(NA, dim = c(n, itrs[2], itrs[3]))
  for (i in seq_len(itrs[3])) {
    for (j in seq_len(itrs[2])) {
      ret[, j, i] <- f(tmp[, j, i])
    }
  }
  if (along != 1L) {
    ret <- aperm(ret, perm_out)
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
    for (i in seq_len(nch))
      for (j in GetAvailEpisodes(abf)) {
        ret[j, i] <- map_func(abf[mask, j, channel[i]], ...)
      }

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
