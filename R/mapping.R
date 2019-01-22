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

#' Mapping function to an nd-array along specific axes.
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

  ret
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

    f_along(abf, intv = intv, episode = NULL, channel = channel)
  }

  f
}

#' @rdname WrapMappingFunc
#' @export
#'
wrap <- function(...) {

  WrapMappingFunc(...)
}

#' Wrap a mapping function along specific axis to batch process abf data.
#'
#' The wrapped function has a signature of f(abf, intv = NULL, episode, channel)
#' see details for more details.
#'
#' Arguments of the wrapped function f(abf, intv = NULL, episode, channel):
#'
#' abf   a abf object
#'
#' intv   OPTIONAL, a time interval to calcaulte. If NULL, the whole timespan is used.
#'
#' episode   OPTIONAL, a vector of episodes to calculate. If missing, all episodes are used.
#'
#' channel   OPTIONAL, a vector of channels to calculate. If missing, all channels are used.
#'
#'
#' Dimensional order of precedence: time < episode < channel (same as abf objects).
#' If the mapping function spans multiple dimensions, result will be permutated
#' to preserve the order. Meanwhile, a warning will be thrown for the first time
#' wrapped function is called, notifying which dim will be expanded for the user.
#'
#' @param map_func a mapping function.
#' @param along the axis to process along. Can be "time" (1L), "episode" (2L) or "channel" (3L).
#' @param pack_args wheter to pack arguments for map_func, see PackArgs() for more details.
#' @param abf_id_func OPTIONAL, a function accepts an abf object and returns an identifier of the objects.
#' @param epi_id_func OPTIONAL, a function accepts an abf object and returns a vector of identifiers of all episodes.
#' @param chan_id_func OPTIONAL, a function accepts an abf object and returns a vectors of identifiers of all channels.
#' @param time_unit convert time unit of the return time axis.
#' @param ret.df wheter to return a data.frame, a matrix is returned instead if set to FALSE
#' @param dim_warn whether to warn about dimension expansion.
#' @param ... further arguments passed to map_func
#'
#' @return a function
#' @export
#'
WrapMappingFuncAlong <- function(map_func, along = "time", pack_args = FALSE,
                                 abf_id_func = NULL, epi_id_func = NULL, chan_id_func = NULL,
                                 time_unit = "tick", ret.df = TRUE, dim_warn = TRUE, ...) {

  if (!is.function(map_func)) {
    err_not_func(map_func)
  }

  msg <- "Use mapnd() to avoid ambiguity in returned dimensions."
  along <- FirstElement(unlist(along), msg)
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

  warned_once <- !dim_warn

  parse_dim_info <- function(abf, mask_time, mask_epi, mask_chan, dim_exp) {

    #unique id
    abf_id <- NULL
    if (!is.null(abf_id_func)) {
      if (is.function(abf_id_func)) {
        abf_id <- abf_id_func(abf)
      } else {
        abf_id <- abf_id_func
      }
    }

    #dim 1
    time_id <- TickToTime(abf, time_unit, mask_time)
    if (dim_exp && time_unit != "tick" && !warned_once) {
      msg <- sprintf("Time dim unit is converted to %s.", time_unit)
      warning(msg)
    }


    #dim 2
    epi_id <- NULL
    if (!is.null(epi_id_func)) {
      if (is.function(epi_id_func)) {
        epi_id <- epi_id_func(abf)[mask_epi]
      } else {
        epi_id <- unlist(epi_id_func)[mask_epi]
      }
    }

    #dim 3
    chan_id <- NULL
    if (!is.null(chan_id_func)) {
      if (is.function(chan_id_func)) {
        chan_id <- chan_id_func(abf)[mask_chan]
      } else {
        chan_id <- unlist(chan_id_func)[mask_chan]
      }
    }

    dim_info <- list()
    dim_info$abf_id <- abf_id
    dim_info$time_id <- time_id
    dim_info$epi_id <- epi_id
    dim_info$chan_id <- chan_id

    dim_info
  }
  parse_col_info <- function(dim_info, mask_epi, mask_chan) {

    col_names <- NULL
    col_extra <- NULL

    #add an extra column of id
    if (!is.null(dim_info$abf_id)) {
      col_names <- "id"
      col_extra <- dim_info$abf_id
    }

    if (along == 1L) {
      #along time, columns -> channels, rows -> episodes
      if (!is.null(dim_info$epi_id)) {
        #add an extra column of episode id if epi_id is present.
        col_names <- c(col_names, "Episode")
        col_extra <- cbind(col_extra, dim_info$epi_id)
      }
      #col_names of values (channels)
      if (is.null(dim_info$chan_id)) {
        dim_info$chan_id <- as.character(mask_chan)
      }
      col_names <- c(col_names, dim_info$chan_id)
    } else {
      #along episode/channel, rows -> time
      #add an extra column of time.
      col_names <- c(col_names, "Time")
      col_extra <- cbind(col_extra, dim_info$time_id)

      if (along == 2L) {
        #col_names of values (channels)
        if (is.null(dim_info$chan_id)) {
          dim_info$chan_id <- as.character(mask_chan)
        }
        col_names <- c(col_names, dim_info$chan_id)
      }

      if (along == 3L) {
        #col_names of values (episodes)
        if (is.null(dim_info$epi_id)) {
          dim_info$epi_id <- as.character(mask_epi)
        }
        col_names <- c(col_names, dim_info$epi_id)
      }
    }

    col_info <- list()
    col_info$col_names <- col_names
    col_info$col_extra <- col_extra

    col_info
  }
  fix_dim <- function(data, dim_info) {

    #fix_dim should also set proper dim names when extra_dim is 0L
    extra_dim <- length(dim(data)) - 2L

    #instead of being collapsed, selected dim is expanded.
    if (!warned_once) {
      #give a warning for the first time called.
      msg_dim <- switch(along, "Time", "Episode", "Channel")
      msg_ndim <- extra_dim
      msg <- sprintf("Returned values of mapping function have lengths > 1. %s axis is expanded to %d dimensions.",
                     msg_dim, msg_ndim)
      warning(msg)
      warned_once <<- TRUE
    }

    #depending on dim along, attach dimnames to returned array.
    dim_name <- dimnames(data)
    if (along == 1L) {
      #..., episode, channel
      dim_name[[1L + extra_dim]] <- dim_info$epi_id
      dim_name[[2L + extra_dim]] <- dim_info$chan_id
      dimnames(data) <- dim_name
      #do not need to permutate
    } else if (along == 2L) {
      #..., time, channel
      dim_name[[1L + extra_dim]] <- dim_info$time_id
      dim_name[[2L + extra_dim]] <- dim_info$chan_id
      dimnames(data) <- dim_name
      #permutate dimensions
      data <- aperm(data, c(1L + extra_dim, seq_len(extra_dim), 2L + extra_dim))
    } else {
      #..., time, episode
      dim_name[[1L + extra_dim]] <- dim_info$time_id
      dim_name[[2L + extra_dim]] <- dim_info$epi_id
      dimnames(data) <- dim_name
      #permutate dimensions
      data <- aperm(data, c(1L + extra_dim, 2L + extra_dim, seq_len(extra_dim)))
    }

    data
  }
  fix_col <- function(data, col_info) {

    if (!is.null(col_info$col_extra)) {
      data <- cbind(col_info$col_extra, data)
    }
    colnames(data) <- col_info$col_names

    data
  }

  f <- function(abf, intv = NULL, episode = NULL, channel = NULL) {

    if (!IsAbf(abf)) {
      err_class_abf()
    }

    mask_time = integer()
    if (is.null(intv) || any(is.na(intv))) {
      mask_time <- seq_len(nPts(abf))
    } else {
      mask_time <- MaskIntv(intv)
    }
    mask_epi = integer()
    if (is.null(episode)) {
      mask_epi <- GetAvailEpisodes(abf)
    } else {
      mask_epi <- as.integer(unlist(episode))
      if (!AssertEpisode(abf, mask_epi)) {
        err_epi()
      }
    }
    mask_chan = integer()
    if (is.null(channel)) {
      mask_chan <- GetAllChannels(abf)
    } else {
      mask_chan <- as.integer(unlist(channel))
      if (!AssertChannel(abf, mask_chan)) {
        err_channel()
      }
    }

    #extract data
    xdata <- abf[mask_time, mask_epi, mask_chan, drop = FALSE]
    #apply to map_func
    ret <- mapnd(x = xdata, f = map_func, along = along, pack_args = pack_args, ...)
    #get dimension info
    dim_exp <- length(dim(ret)) > 2L
    dim_info <- parse_dim_info(abf, mask_time, mask_epi, mask_chan, dim_exp)

    if (dim_exp) {
      ret <- fix_dim(ret, dim_info)
    } else {
      col_info <- parse_col_info(dim_info, mask_epi, mask_chan)
      if (ret.df) {
        ret <- as.data.frame(ret)
      }
      ret <- fix_col(ret, col_info)
    }

    ret <- CpChannelAttr(ret, abf, channel)

    ret
  }

  f
}

#' @rdname WrapMappingFuncAlong
#' @export
#'
wrap_along <- function(..., abf_id_func = NULL,
                       epi_id_func = DefaultEpiLabel,
                       chan_id_func = DefaultChanLabel,
                       time_unit = "ms", ret.df = TRUE){

  WrapMappingFuncAlong(..., abf_id_func = abf_id_func, epi_id_func = epi_id_func,
                       chan_id_func = chan_id_func, time_unit = time_unit,
                       ret.df = ret.df)
}
