#' Calculate column standard error of measurements
#'
#' @param df a data frame of data frame like 2d data
#' @param na.rm remove NAs
#'
#' @return calculated column sem
#' @export
#'
colSems <- function(df, na.rm = FALSE) {

  sds <- matrixStats::colSds(df, na.rm = na.rm)
  sqn <- sqrt(length(sds))

  return(sds / sqn)
}

#' Generate an interval.
#'
#' Only two or the arguments are needed. If all are given, len will be ignored.
#'
#' @param startPos start position.
#' @param endPos end position.
#' @param len length of the interval.
#'
#' @return an interval.
#' @export
#'
Intv <- function(startPos, endPos, len) {

  if (missing(startPos)) {
    startPos <- endPos - len + 1L
  }
  if (missing(endPos)) {
    endPos <- startPos + len - 1L
  }
  if (startPos >= endPos) {
    err_intv_pos()
  }
  len <- endPos - startPos + 1L

  return(c(startPos, endPos, len))
}

#' Set intervals, by-ref behaviour
#'
#' @param intv an interval.
#' @param startPos starting position of the new interval.
#' @param endPos ending position of the new interval.
#'
#' @return an interval.
#' @export
#'
SetIntv <- function(intv, startPos, endPos) {

  if (startPos >= endPos) {
    err_intv_pos()
  }
  if (abs(endPos - startPos) < 3L) {
    warning("SetIntv: interval size smaller than 3.")
  }

  eval.parent(substitute({
    intv <- c(startPos, endPos, endPos - startPos + 1L)
  }))
}

push <- function(x, items) {

  if (is.vector(x) || is.list(x)) {
    eval.parent(substitute({
      x <- append(x, items)
    }))
  } else {
    err_class_vec_list()
  }
}

pop <- function(x) {

  #this contaminates parent's env, need better solution
  if (is.vector(x)) {
    eval.parent(substitute({
      item <- x[length(x)]
      x <- x[-length(x)]
      item
    }))
  } else if (is.list(x)) {
    eval.parent(substitute({
      item <- x[[length(x)]]
      x <- x[[-length(x)]]
      item
    }))
  } else {
    err_class_vec_list()
  }
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


#' Apply a function over a list of abf objects.
#'
#' @param abf_list a list of abf objects.
#' @param FUN the function to be applied to each episode of the abf objects.
#' @param ... further arguments passed to FUN.
#' @param channel channel of the abf objects to apply.
#' @param intv OPTIONAL, the interval to apply FUN.
#' @param gen_names whether to generate row names.
#'
#' @return a matrix, of which each row represents an episode and column represents an abf object in the list.
#' @export
#'
abfapply <- function(abf_list, FUN, ..., channel = 1L, intv = NULL, gen_names = FALSE) {

  if (!IsAbfList(abf_list)) {
    err_class_abf_list()
  }
  channel <- FirstElement(unlist(channel))
  for (tmp in abf_list) {
    if (!AssertChannel(tmp ,channel)) {
      err_channel()
    }
  }

  intv <- ExpandList(intv, abf_list)
  if (is.null(intv)) {
    err_assert_len(intv, abf_list)
  }

  f <- WrapMappingFunc(FUN, abf_id_func = NULL, epi_id_func = NULL,
                       chan_id_func = NULL, channel = channel, ...)
  ret <- NULL
  for (i in seq_along(abf_list)) {
    ret <- cbind(ret, f(abf_list[[i]], intv[[i]]))
  }
  colnames(ret) <- unlist(GetTitle(abf_list))

  if (gen_names) {
    rownames(ret) <- DefaultEpiLabel(abf_list[[1]])
  }

  return(ret)
}
