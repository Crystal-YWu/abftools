#' Get title of an abf object.
#'
#' @param abf an abf object.
#'
#' @return the title of the abf object.
#' @export
#'
GetTitle <- function(abf) {

  if (IsAbf(abf)) {
    return(attr(abf, "title"))
  } else if (IsAbfList(abf)) {
    return(lapply(abf, function(x) attr(x, "title")))
  } else {
    err_class_abf_list()
  }

}

#' Set title of an abf object, by-ref behaviour.
#'
#' In order to mimic the by-ref behaviour, when setting titles for a list of abf
#' objects, a temp variable "i_____" in caller's scope is used and then removed.
#' This may cause some trouble if you use i_____ in your code.
#'
#' @param abf an abf object, a list of abf objects are also supported.
#' @param title the title to be set.
#'
#' @return an abf object
#' @export
#'
SetTitle <- function(abf, title) {

  if (IsAbf(abf)) {
    eval.parent(substitute({
      attr(abf, "title") <- as.character(title)
      invisible(abf)
    }))
  } else if (IsAbfList(abf)) {
    if (!AssertLength(title, abf, explicit = 1L)) {
      err_assert_len(title, abf)
    }
    if (length(title) == 1L) {
      eval.parent(substitute({
        for (i_____ in seq_along(abf)) {
          attr(abf[[i_____]], "title") <- as.character(title)
        }
        rm(i_____)
        invisible(abf)
      }))
    } else {
      eval.parent(substitute({
        for (i_____ in seq_along(abf)) {
          attr(abf[[i_____]], "title") <- as.character(title[[i_____]])
        }
        rm(i_____)
        invisible(abf)
      }))
    }
  } else {
    err_class_abf_list()
  }
}


#' Get names of channels.
#'
#' @param abf an abf object.
#'
#' @return a character vector of channel names.
#' @export
#'
GetChannelName <- function(abf) {

  if (!IsAbf(abf)) {
    err_class_abf()
  }

  return(attr(abf, "ChannelName"))
}

#' Get units of channels.
#'
#' @param abf an abf object.
#'
#' @return a character vector of channel units.
#' @export
#'
GetChannelUnit <- function(abf) {

  if (!IsAbf(abf)) {
    err_class_abf()
  }

  return(attr(abf, "ChannelUnit"))
}

#' Get descriptions of channels.
#'
#' @param abf an abf object.
#'
#' @return a character vector of channel descriptions.
#' @export
#'
GetChannelDesc <- function(abf) {

  if (!IsAbf(abf)) {
    err_class_abf()
  }

  return(attr(abf, "ChannelDesc"))
}

#' Get sampling interval in us.
#'
#' @param abf an abf object.
#'
#' @return the sampling interval in unit us.
#' @export
#'
GetSamplingIntv <- function(abf) {

  if (!IsAbf(abf)) {
    err_class_abf()
  }

  return(attr(abf, "SamplingInterval"))
}

#' Get mode of the abf object.
#'
#' @param abf an abf object.
#'
#' @return the mode of the recording.
#' @export
#'
GetMode <- function(abf) {

  if (!IsAbf(abf)) {
    err_class_abf()
  }

  return(attr(abf, "mode"))
}

#' Get number of channels.
#'
#' @param abf an abf object.
#'
#' @return number of channels.
#' @export
#'
GetNumOfChannel <- function(abf) {

  meta <- get_meta(abf)
  #Every observation of table ADC is a channel
  ret <- nrow(meta$ADC)

  return(ret)
}

#' Get number of episodes/sweeps per channel.
#'
#' The returned number may be different to the result of dim(abf[[chan_id]]) if
#' you have removed episodes from the abf object. GetEpisodesPerChannel / nEpi
#' always return the original number of episodes per channel setting in the
#' abf2 protocol.
#'
#' @param abf an abf object.
#'
#' @return number of episodes/sweeps per channel.
#' @export
#'
GetEpisodesPerChannel <- function(abf) {

  mode <- GetMode(abf)
  if (mode == 3L) {
    return(1L)
  }

  meta <- get_meta(abf)
  ret <- nrow(meta$SynchArray)

  return(ret)
}

#' Get number of recorded points per episode/sweep.
#'
#' @param abf an abf object.
#'
#' @return number of recorded points per episode/sweep.
#' @export
#'
GetPointsPerEpisode <- function(abf) {

  mode <- GetMode(abf)
  if (mode == 3L) {
    return(dim(abf)[1])
  }

  meta <- get_meta(abf)
  if (mode == 1L) {
    ret <- meta$SynchArray$lLength %/% GetNumOfChannel(abf)
  } else {
    ret <- meta$SynchArray$lLength[1] %/% GetNumOfChannel(abf)
  }

  return(ret)
}

#' Get number of epochs.
#'
#' @param abf an abf object
#'
#' @return number of epochs.
#' @export
#'
GetNumOfEpoch <- function(abf) {

  meta <- get_meta(abf)
  epdac <- meta$EpochPerDAC
  if (is.null(epdac)) {
    return(0L)
  }

  return(nrow(epdac))
}

#' Get number of channels.
#'
#' @param abf an abf object.
#'
#' @return number of channels.
#' @export
#'
nChan <- function(abf) {

  return(GetNumOfChannel(abf))
}

#' Get number of recorded points per episode/sweep.
#'
#' @param abf an abf object.
#'
#' @return number of recorded points per episode/sweep.
#' @export
#'
nPts <- function(abf) {

  return(GetPointsPerEpisode(abf))
}

#' Get number of episodes/sweeps per channel.
#'
#' The returned number may be different to the result of dim(abf[[chan_id]]) if
#' you have removed episodes from the abf object. GetEpisodesPerChannel / nEpi
#' always return the original number of episodes per channel setting in the
#' abf2 protocol.
#'
#' @param abf an abf object.
#'
#' @return number of episodes/sweeps per channel.
#' @export
#'
nEpi <- function(abf) {

  return(GetEpisodesPerChannel(abf))
}

#' Get number of epochs.
#'
#' @param abf an abf object
#'
#' @return number of epochs.
#' @export
#'
nEpoch <- function(abf) {

  return(GetNumOfEpoch(abf))
}
