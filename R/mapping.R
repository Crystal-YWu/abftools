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
#' @param along the axis to map the function to (dims to collapse). Ignored if MARGIN is given
#' @param MARGIN OPTIONAL, the dimensions to apply f over (dims to preserve).
#' @param pack_args whether to pack arguments for f.
#' @param ... other argumetns passed to f.
#'
#' @return an array (dimension depending on returned dimension of f)
#' @export
#'
mapnd <- function(x, f, along = 1L, MARGIN = NULL, pack_args = FALSE, ...) {

  margin <- MARGIN
  if (is.null(MARGIN)) {
    ndim <- seq_len(length(dim(x)))
    margin <- ndim[-along]
  }

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
#' @param channel OPTIONAL, a channel/channels to process.
#' @param abf_id_func OPTIONAL, a function accepts an abf object and returns an identifier of the objects.
#' @param epi_id_func OPTIONAL, a function accepts an abf object and returns a vector of identifiers of all episodes.
#' @param chan_id_func OPTIONAL, a function accepts an abf object and returns a vectors of identifiers of all channels.
#' @param ret.df wheter to return a data.frame, a matrix is returned instead if set to FALSE
#' @param ... further arguments passed to map_func
#'
#' @return a function of f(abf, intv).
#' @export
#'
WrapMappingFunc <- function(map_func, channel, abf_id_func = NULL, epi_id_func = NULL,
                            chan_id_func = DefaultChanLabel, ret.df = TRUE, ...) {

  if (missing(channel)) {
    channel <- NULL
  }
  f_along <- WrapMappingFuncAlong(map_func = map_func, abf_id_func = abf_id_func,
                                  epi_id_func = epi_id_func, chan_id_func = chan_id_func,
                                  along = 1L, ret.df = ret.df, ...)

  f <- function(abf, intv = NULL) {

    ret <- f_along(abf, intv = intv, episode = NULL, channel = channel)

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

#' Wrap a mapping function along specific axis to batch process abf data.
#'
#' @param map_func a mapping function.
#' @param along the axis to process along. Can be "time" (1L), "episode" (2L) or "channel" (3L).
#' @param pack_args wheter to pack arguments for map_func, see PackArgs() for more details.
#' @param abf_id_func OPTIONAL, a function accepts an abf object and returns an identifier of the objects.
#' @param epi_id_func OPTIONAL, a function accepts an abf object and returns a vector of identifiers of all episodes.
#' @param chan_id_func OPTIONAL, a function accepts an abf object and returns a vectors of identifiers of all channels.
#' @param time_unit convert time unit of the return time axis.
#' @param ret.df wheter to return a data.frame, a matrix is returned instead if set to FALSE
#' @param ... further arguments passed to map_func
#'
#' @return a function of f(abf, intv, episode, channel)
#' @export
#'
WrapMappingFuncAlong <- function(map_func, along = "time", pack_args = FALSE,
                                 abf_id_func = NULL, epi_id_func = NULL, chan_id_func = NULL,
                                 time_unit = "tick", ret.df = TRUE, ...) {

  if (!is.function(map_func)) {
    err_not_func(map_func)
  }

  along <- FirstElement(unlist(along))
  if (is.character(along)) {
    along <- switch(substr(toupper(along), 1L, 1L),
                    T = 1L,
                    E = 2L,
                    C = 3L,
                    err_invalid_axis(along))
  }
  if (along > 3L || along < 1L) {
    err_invalid_axis(along)
  }

  f <- function(abf, intv = NULL, episode, channel) {

    if (!IsAbf(abf)) {
      err_class_abf()
    }

    mask_time = integer()
    if (is.null(intv)) {
      mask_time <- seq_len(nPts(abf))
    } else {
      mask_time <- MaskIntv(intv)
    }
    mask_epi = integer()
    if (missing(episode) || is.null(episode)) {
      mask_epi <- GetAvailEpisodes(abf)
    } else {
      mask_epi <- as.integer(unlist(episode))
      if (!AssertEpisode(abf, mask_epi)) {
        err_epi()
      }
    }
    mask_chan = integer()
    if (missing(channel) || is.null(channel)) {
      mask_chan <- GetAllChannels(abf)
    } else {
      mask_chan <- as.integer(unlist(channel))
      if (!AssertChannel(abf, mask_chan)) {
        err_channel()
      }
    }

    xdata <- abf[mask_time, mask_epi, mask_chan, drop = FALSE]
    ret <- mapnd(x = xdata, f = map_func, along = along, pack_args = pack_args, ...)
    if (length(dim(ret)) > 2L) {
      err_wrap_func_dim(map_func)
    }
    if (ret.df) {
      ret <- as.data.frame(ret)
    }

    #time id is implicit only if along != 1L, and the only id doesn't change
    #ret type to data.frame since numeric
    time_id <- TickToTime(abf, time_unit, mask_time)
    #character ids
    abf_id <- NULL
    if (!is.null(abf_id_func)) {
      if (is.function(abf_id_func)) {
        abf_id <- abf_id_func(abf)
      } else {
        abf_id <- abf_id_func
      }
    }
    epi_id <- NULL
    if (!is.null(epi_id_func)) {
      if (is.function(epi_id_func)) {
        epi_id <- epi_id_func(abf)[mask_epi]
      } else {
        epi_id <- unlist(epi_id_func)[mask_epi]
      }
    }
    chan_id <- NULL
    if (!is.null(chan_id_func)) {
      if (is.function(chan_id_func)) {
        chan_id <- chan_id_func(abf)[mask_chan]
      } else {
        chan_id <- unlist(chan_id_func)[mask_chan]
      }
    }

    col_names <- NULL
    col_ids <- NULL
    if (!is.null(abf_id)) {
      col_names <- "id"
      col_ids <- abf_id
    }
    if (along == 1L) {
      #dim time is collapsed
      if (!is.null(epi_id)) {
        col_names <- c(col_names, "Episode")
        col_ids <- cbind(col_ids, epi_id)
      }
      #col_names of values
      if (is.null(chan_id)) {
        chan_id <- as.character(mask_chan)
      }
      col_names <- c(col_names, chan_id)
    } else {
      #time id is always present when not along 1L
      col_names <- c(col_names, "Time")
      col_ids <- cbind(col_ids, time_id)
      if (along == 2L) {
        #col_names of values
        if (is.null(chan_id)) {
          chan_id <- as.character(mask_chan)
        }
        col_names <- c(col_names, chan_id)
      }
      if (along == 3L) {
        #col_names of values
        if (is.null(epi_id)) {
          epi_id <- as.character(mask_epi)
        }
        col_names <- c(col_names, epi_id)
      }
    }
    if (!is.null(col_ids)) {
      ret <- cbind(col_ids, ret)
    }
    colnames(ret) <- col_names

    return(ret)
  }

  return(f)
}

#' Convenient alias of WrapMappingFuncAlong()
#'
#' @param ... See WrapMappingFuncAlong for more details.
#'
#' @return a function of f(abf, intv, episode, channel)
#' @export
#'
wrap_along <- function(..., abf_id_func = NULL,
                       epi_id_func = DefaultEpiLabel,
                       chan_id_func = DefaultChanLabel,
                       time_unit = "ms", ret.df = TRUE){

  return(WrapMappingFuncAlong(..., abf_id_func = abf_id_func, epi_id_func = epi_id_func,
                              chan_id_func = chan_id_func, time_unit = time_unit,
                              ret.df = ret.df))
}
