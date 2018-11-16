MeltAbfMean <- function(abflist, intvlist) {

  domean <- function(abf, intv) {
    ret <- mean(abf, intv)
    ret <- cbind(GetTitle(abf), ret)
    colnames(ret)[1] <- "id"

    return(ret)
  }

  melted <- NULL
  if (missing(intvlist) || is.null(intvlist)) {
    intvlist <- rep(NULL, length(abflist))
  }
  for (i in seq_along(abflist)) {
    if (is.null(melted)) {
      melted <- domean(abflist[[i]], intvlist[[i]])
    }
    tmp <- domean(abflist[[i]], intvlist[[i]])
    melted <- rbind(melted, tmp)
  }

  return(melted)
}

#' Melt channel data of abf objects.
#'
#' @param abf an abf/list of abf objects.
#' @param channel channel/channels to melt.
#' @param intv OPTIONAL, an interval to process.
#' @param map_func a mapping function to process data.
#' @param abf_id_func OPTIONAL, a function accepts an abf object and returns an identifier of the objects.
#' @param epi_id_func OPTIONAL, a function accepts an abf object and returns a vector of identifiers of all episodes.
#' @param chan_id_func OPTIONAL, a function accepts an abf object and returns a vectors of identifiers of all channels.
#' @param ... further arguments passed to map_func.
#'
#' @return a melted data frame
#' @export
#'
MeltAbfChannel <- function(abf, channel, intv = NULL,
                           map_func = mean,
                           abf_id_func = GetTitle,
                           epi_id_func = DefaultEpiLabel,
                           chan_id_func = DefaultChanLabel, ...) {

  #convert abf, intv to lists and check lengths
  if (IsAbf(abf)) {
    abf <- list(abf)
  } else if (!IsAbfList(abf)) {
    err_class_abf_list()
  }
  #check channel
  if (missing(channel) || is.null(channel)) {
    #abf is now a list of abf instead, so we use abf[[1]] instead
    channel <- GetAllChannels(abf[[1]])
  }
  channel <- unlist(channel)
  for (tmp in abf) {
    if (!AssertChannel(tmp, channel)) {
      err_channel()
    }
  }

  intv <- ExpandList(intv, abf)
  if (is.null(intv)) {
    err_assert_len(intv, abf)
  }

  #check functions
  if (!is.function(map_func)) {
    err_not_func(map_func)
  }
  if (!is.null(abf_id_func) && !is.function(abf_id_func)) {
    err_not_func(abf_id_func)
  }
  if (!is.null(epi_id_func) && !is.function(epi_id_func)) {
    err_not_func(epi_id_func)
  }
  if (!is.null(chan_id_func) && !is.function(chan_id_func)) {
    err_not_func(chan_id_func)
  }

  #check channel ids
  channel_id <- chan_id_func(abf[[1]])[channel]
  for (tmp in abf) {
    tmp <- chan_id_func(tmp)[channel]
    if (!all(channel_id == tmp)) {
      err_chan_id()
    }
  }

  f <- WrapMappingFunc(map_func, abf_id_func, epi_id_func, chan_id_func,
                       channel, ...)
  melted <- NULL
  for (i in seq_along(abf)) {
    tmp <- f(abf[[i]], intv[[i]])
    melted <- rbind(melted, tmp)
  }

  return(melted)
}
