#' Melt an abf object.
#'
#' Melt an abf object into a data.frame grouped by episodes. A time column with
#' unit provided by time_unit is added to encode time dimension of the orignal
#' abf object.
#'
#' For performance consideration, using a column function by sampling_colFunc is
#' strongly encouraged.
#'
#' @param abf an abf object.
#' @param intv a time interval to melt.
#' @param channel channels to melt.
#' @param sampling_ratio a sampling ratio if need to reduce data points.
#' @param sampling_func a sampling function if sampling_ratio > 1.
#' @param sampling_colFunc a column vectorised sampling function, use instead of
#' sampling_func for better performance.
#' @param time_unit a time unit.
#' @param ... passed to sampling_func or sampling_colFunc.
#' @param value.name a character vector to identity channel columns.
#'
#' @return a data.frame
#' @export
#'
MeltAbf <- function(abf, intv = NULL, channel = 1L,
                    sampling_ratio = 1L, sampling_func = NULL, sampling_colFunc = NULL,
                    time_unit = c("tick", "us", "ms", "s", "min", "hr"), ...,
                    value.name = NULL) {

  CheckArgs(abf, chan = channel)

  time_unit <- match.arg(time_unit)

  epi <- GetAvailEpisodes(abf)
  nepi <- length(epi)

  #map
  if (is.null(intv)) {
    npts <- nPts(abf)
    t_start <- 1L
    t_end <- npts
    if (!is.null(sampling_colFunc)) {
      data <- samplend_col(x = abf[, epi, channel],
                           ratio = sampling_ratio,
                           colFunc = sampling_colFunc,
                           along = 1L,
                           ...)
    } else {
      data <- samplend(x = abf[, epi, channel],
                       ratio = sampling_ratio,
                       func = sampling_func,
                       along = 1L,
                       ...)
    }
  } else {
    mask <- MaskIntv(intv)
    npts <- length(mask)
    t_start <- mask[1L]
    t_end <- mask[npts]
    if (!is.null(sampling_colFunc)) {
      data <- samplend_col(x = abf[mask, epi, channel],
                           ratio = sampling_ratio,
                           colFunc = sampling_colFunc,
                           along = 1L,
                           ...)
    } else {
      data <- samplend(x = abf[mask, epi, channel],
                       ratio = sampling_ratio,
                       func = sampling_func,
                       along = 1L,
                       ...)
    }
  }

  #reduce
  if (is.null(value.name)) {
    chan <- GetChannelDesc(abf, channel = channel)
  } else {
    if (length(value.name) == length(channel)) {
      chan <- as.character(value.name)
    } else {
      chan <- sprintf("%s%d", value.name, channel)
    }
  }
  data <- reduce_lastdim(data, cols = chan)

  tick <- seq.int(from = t_start, to = t_end, by = sampling_ratio)
  time <- rep(TickToTime(abf, time_unit, tick), nepi)

  ep <- as.factor(matrix(epi, nrow = length(tick), ncol = nepi, byrow = TRUE))
  levels(ep) <- DefaultEpiLabel(epi)

  time_ep <- list(
    Time = time,
    Episode = ep
  )

  data.frame(c(time_ep, data))
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
#' @param map_func a mapping function.
#' @param along the axis to process along. Can be "time", "episode" or "channel".
#' @param pack_args wheter to pack arguments for map_func, see PackArgs() for more details.
#' @param abf_id_func OPTIONAL, a function accepts an abf object and returns an identifier of the objects.
#' @param epi_id_func OPTIONAL, a function accepts an abf object and returns a vector of identifiers of all episodes.
#' @param chan_id_func OPTIONAL, a function accepts an abf object and returns a vectors of identifiers of all channels.
#' @param time_unit convert time unit of the return time axis.
#' @param ret.df wheter to return a data.frame, a matrix is returned instead if set to FALSE.
#' @param ... further arguments passed to map_func.
#'
#' @return a function
#' @export
#'
WrapMappingFuncAlong <- function(map_func, along = c("time", "episode", "channel"), pack_args = FALSE,
                                 abf_id_func = NULL, epi_id_func = DefaultEpiLabel, chan_id_func = DefaultChanLabel,
                                 time_unit = "tick", ret.df = TRUE, ...) {

  along <- match.arg(along)
  along <- switch(along, time = 1L, episode = 2L, channel = 3L)

  dim_row <- switch(along, 2L, 1L, 1L)
  dim_col <- switch(along, 3L, 3L, 2L)
  dim_id <- c("Time", "Episode", "Channel")

  get_dim_names <- function(abf, mask_time, mask_epi, mask_chan) {
    ans <- list()
    #dim 1
    if (along == 1L) {
      #do not generate dim 1 if along time
      ans[[1]] <- NA
    } else {
      ans[[1]] <- TickToTime(abf, time_unit, mask_time)
    }
    #dim 2
    if (is.null(epi_id_func)) {
      ans[[2]] <- mask_epi
    } else {
      if (is.function(epi_id_func)) {
        ans[[2]] <- epi_id_func(abf)[mask_epi]
      } else {
        ans[[2]] <- unlist(epi_id_func)[mask_epi]
      }
    }
    #dim 3
    if (is.null(chan_id_func)) {
      ans[[3]] <- mask_chan
    } else {
      if (is.function(chan_id_func)) {
        ans[[3]] <- chan_id_func(abf)[mask_chan]
      } else {
        ans[[3]] <- unlist(chan_id_func)[mask_chan]
      }
    }
    #id
    if (!is.null(abf_id_func)) {
      if (is.function(abf_id_func)) {
        ans$id <- abf_id_func(abf)
      } else {
        ans$id <- unlist(abf_id_func)
      }
    }
    ans
  }

  dots <- list(...)
  f <- function(abf, intv = NULL,
                episode = GetAvailEpisodes(abf),
                channel = GetAllChannels(abf)){

    mask_epi <- as.integer(unlist(episode))
    mask_chan <- as.integer(unlist(channel))
    CheckArgs(abf,  epi = mask_epi, chan = mask_chan)

    if (is.null(intv) || any(is.na(intv))) {
      mask_time <- seq_len(nPts(abf))
    } else {
      mask_time <- MaskIntv(intv)
    }

    #map
    args <- c(list(
      x = abf[mask_time, mask_epi, mask_chan, drop = FALSE],
      func = map_func,
      along = along,
      pack_args = pack_args
    ), dots)
    ans <- do.call(mapnd, args)

    if (length(dim(ans)) > 2L) {
      err_wrap_func_dim(map_func)
    }

    #reduce
    dim_names <- get_dim_names(abf, mask_time, mask_epi, mask_chan)
    ans <- reduce_lastdim(ans, col = dim_names[[dim_col]])

    #extra id columns
    xcol <- list()
    if (!is.null(dim_names$id)) {
      xcol$id <- dim_names$id
    }
    xcol[[dim_id[dim_row]]] <- dim_names[[dim_row]]

    if (ret.df) {
      data.frame(c(xcol, ans), check.names = FALSE)
    } else {
      do.call(cbind, ans)
    }
  }

  f
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
#' @param ret.df wheter to return a data.frame, a matrix is returned instead if set to FALSE.
#' @param ... further arguments passed to map_func.
#'
#' @return a function of f(abf, intv).
#' @export
#'
WrapMappingFunc <- function(map_func, channel,
                            abf_id_func = NULL,
                            epi_id_func = DefaultEpiLabel,
                            chan_id_func = DefaultChanLabel, ret.df = TRUE, ...) {

  f_along <- WrapMappingFuncAlong(map_func = map_func,
                                  abf_id_func = abf_id_func,
                                  epi_id_func = epi_id_func,
                                  chan_id_func = chan_id_func,
                                  along = "time", ret.df = ret.df, ...)

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

#' @rdname WrapMappingFuncAlong
#' @export
#'
wrap_along <- function(...,
                       time_unit = "ms",
                       ret.df = TRUE) {


  WrapMappingFuncAlong(..., time_unit = time_unit, ret.df = ret.df)
}
