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
WrapMappingFunc <- function(map_func, channel,
                            abf_id_func = NULL,
                            epi_id_func = NULL,
                            chan_id_func = DefaultChanLabel, ret.df = TRUE, ...) {

  f_along <- WrapMappingFuncAlong(map_func = map_func,
                                  abf_id_func = abf_id_func,
                                  epi_id_func = epi_id_func,
                                  chan_id_func = chan_id_func,
                                  along = 1L, ret.df = ret.df, ...)

  ch_missing <- missing(channel)
  f <- function(abf, intv = NULL) {

    if (ch_missing) {
      f_along(abf, intv = intv)
    } else {
      f_along(abf, intv = intv, channel = channel)
    }
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

  get_dim_ids <- function(abf, mask_time, mask_epi, mask_chan, dim_exp) {

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
    if (dim_exp && along != 1L && time_unit != "tick" && !warned_once) {
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

    list(
      abf_id = abf_id,
      time_id = time_id,
      epi_id = epi_id,
      chan_id = chan_id
    )
  }
  get_col_info <- function(dim_ids, mask_epi, mask_chan) {

    #add an extra column of id
    if (is.null(dim_ids$abf_id)) {
      col_extra <- list()
    } else {
      col_extra <- list(id = dim_ids$abf_id)
    }

    if (along == 1L) {
      #along time, columns -> channels, rows -> episodes
      if (!is.null(dim_ids$epi_id)) {
        #add an extra column of episode id if epi_id is present.
        col_extra <- c(col_extra, list(Episode = dim_ids$epi_id))
      }
      #col_names of values (channels)
      if (is.null(dim_ids$chan_id)) {
        dim_ids$chan_id <- as.character(mask_chan)
      }
      col_names <- dim_ids$chan_id
    } else {
      #along episode/channel, rows -> time

      #add an extra column of time.
      col_extra <- c(col_extra, list(Time = dim_ids$time_id))

      if (along == 2L) {
        #col_names of values (channels)
        if (is.null(dim_ids$chan_id)) {
          dim_ids$chan_id <- as.character(mask_chan)
        }
        col_names <- dim_ids$chan_id
      } else {
        #col_names of values (episodes)
        if (is.null(dim_ids$epi_id)) {
          dim_ids$epi_id <- as.character(mask_epi)
        }
        col_names <- dim_ids$epi_id
      }
    }

    list(
      col_names = col_names,
      col_extra = do.call(cbind, col_extra)
    )
  }
  fix_dim <- function(data, dim_info) {

    #fix_dim should also set proper dim names when extra_dim is 0L
    extra_dim <- length(dim(data)) - 2L

    #instead of being collapsed, selected dim is expanded.
    if (!warned_once) {
      #give a warning for the first time called.
      msg_dim <- switch(along, "Time", "Episode", "Channel")
      if (extra_dim == 1L) {
        msg <- sprintf("Mapping function returned multiple values. %s axis is preserved for returned values.")
      } else {
        msg <- sprintf("Returned values of mapping function is multi-dimensional. %s axis is expanded to %d dimensions.")
      }
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

    if (ret.df) {
      data <- as.data.frame(data)
    }
    colnames(data) <- col_info$col_names
    if (!is.null(col_info$col_extra)) {
      data <- cbind(col_info$col_extra, data)
    }

    data
  }

  dots <- list(...)
  f <- function(abf, intv = NULL,
                episode = GetAvailEpisodes(abf),
                channel = GetAllChannels(abf)) {

    mask_epi <- as.integer(unlist(episode))
    mask_chan <- as.integer(unlist(channel))
    CheckArgs(abf,  epi = mask_epi, chan = mask_chan)

    if (is.null(intv) || any(is.na(intv))) {
      mask_time <- seq_len(nPts(abf))
    } else {
      mask_time <- MaskIntv(intv)
    }

    xdata <- abf[mask_time, mask_epi, mask_chan, drop = FALSE]
    args <- c(list(
      x = xdata,
      f = map_func,
      along = along,
      pack_args = pack_args
      ), dots)
    ret <- do.call(mapnd, args)

    dim_exp <- length(dim(ret)) > 2L
    dim_ids <- get_dim_ids(abf, mask_time, mask_epi, mask_chan, dim_exp)

    if (dim_exp) {
      ret <- fix_dim(ret, dim_ids)
    } else {
      col_info <- get_col_info(dim_ids, mask_epi, mask_chan)
      ret <- fix_col(ret, col_info)
    }

    CpChannelAttr(ret, abf, channel)
  }

  f
}

#' @rdname WrapMappingFuncAlong
#' @export
#'
wrap_along <- function(...,
                       abf_id_func = NULL,
                       epi_id_func = DefaultEpiLabel,
                       chan_id_func = DefaultChanLabel,
                       time_unit = "ms",
                       ret.df = TRUE){

  WrapMappingFuncAlong(..., abf_id_func = abf_id_func, epi_id_func = epi_id_func,
                       chan_id_func = chan_id_func, time_unit = time_unit,
                       ret.df = ret.df)
}

#sample a 3d data structure along dim 1
Sample3d_dim1 <- function(x, sampling_ratio, sampling_func = NULL, ...) {

  force(sampling_ratio)

  d <- dim(x)
  nch <- d[3]
  nepi <- d[2]
  npts <- d[1]

  if (sampling_ratio == 1L) {
    data <- x
  } else {
    idx_smpl <- seq(from = 1L, to = npts, by = sampling_ratio)
    nsmpl <- length(idx_smpl)
    data <- array(x[idx_smpl, , ], dim = c(nsmpl, nepi, nch))
  }

  if (sampling_ratio > 1L && !is.null(sampling_func)) {
    for (ch in seq_len(nch)) {
      for (epi in seq_len(nepi)) {
        if (npts %% sampling_ratio == 0) {
          block_data <- x[, epi, ch]
          dim(block_data) <- c(sampling_ratio, npts %/% sampling_ratio)
          block_sv <- sampling_func(block_data, ...)
          data[, epi, ch] <- block_sv
        } else {
          #divisor
          block_idx <- idx_smpl[nsmpl] - 1L
          block_data <- x[seq_len(block_idx), epi, ch]
          dim(block_data) <- c(sampling_ratio, block_idx %/% sampling_ratio)
          block_sv <- sampling_func(block_data, ...)
          #remainder
          block_last <- x[seq(idx_smpl[nsmpl], npts), epi, ch]
          dim(block_last) <- c(length(block_last), 1L)
          last_sv <- sampling_func(block_last, ...)

          data[, epi, ch] <- c(block_sv, last_sv)
        }
      }
    }
  }

  data
}

#' Melt an abf object.
#'
#' Melt an abf object into a data.frame grouped by episodes. A time column with
#' unit provided by time_unit is added to encode time dimension of the orignal
#' abf object.
#'
#' For performance consideration, sampling_func should be a vectorised col function
#' such as colMeans etc, which apply sampling functions by columns of episodes.
#'
#' @param abf an abf object.
#' @param intv a time interval to melt, default is the whole timespan of abj
#' @param channel channels to melt.
#' @param sampling_ratio sampling ratio.
#' @param sampling_func sampling function.
#' @param time_unit time unit of the melted data, can be tick, us, ms, s, min or hr.
#' @param ... further arguments passed to sampling_func.
#' @param value.name a string to identify/name channel columns, default is named
#' by channel description without unit.
#'
#' @return a melted data frame.
#' @export
#'
MeltAbf <- function(abf, intv = NULL, channel = 1L,
                    sampling_ratio = 1L, sampling_func = NULL,
                    time_unit = "tick", ..., value.name = NULL) {

  CheckArgs(abf, chan = channel)

  epi <- GetAvailEpisodes(abf)
  nepi <- length(epi)

  dots <- list(...)
  if (is.null(intv)) {
    npts <- nPts(abf)
    t_start <- 1L
    t_end <- npts
    args <- c(list(
      x = abf[, epi, channel, drop = FALSE],
      sampling_ratio = sampling_ratio,
      sampling_func = sampling_func
    ), dots)
    data <- do.call(Sample3d_dim1, args)
  } else {
    mask <- MaskIntv(intv)
    npts <- length(mask)
    t_start <- mask[1L]
    t_end <- mask[npts]
    args <- c(list(
      x = abf[mask, epi, channel, drop = FALSE],
      sampling_ratio = sampling_ratio,
      sampling_func = sampling_func
    ), dots)
    data <- do.call(Sample3d_dim1, args)
  }

  value <- lapply(seq_along(channel), function(idx) {
    tmp <- data[,,idx]
    dim(tmp) <- NULL
    tmp
  })
  if (is.null(value.name)) {
    names(value) <- GetChannelDesc(abf)[channel]
  } else {
    value.name <- FirstElement(value.name)
    names(value) <- sprintf("%s%d", as.character(value.name), channel)
  }

  tick <- seq.int(from = t_start, to = t_end, by = sampling_ratio)
  time <- rep(abftools:::TickToTime(abf, time_unit, tick), nepi)
  epi <- matrix(epi, nrow = length(tick), ncol = nepi, byrow = TRUE)
  dim(epi) <- NULL

  ans <- list()
  ans$time <- time
  ans$Episode <- epi

  as.data.frame(do.call(cbind, c(ans, value)))
}
