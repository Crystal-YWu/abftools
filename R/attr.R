#' Get title of an abf object.
#'
#' @param abf an abf object.
#'
#' @return the title of the abf object.
#' @export
#'
GetTitle <- function(abf) {

  if (class(abf) == "abf") {
    return(attr(abf, "title"))
  } else if (IsAbfList(abf)) {
    return(lapply(abf, function(x) attr(x, "title")))
  } else {
    err_class_abf_list("GetTitle")
  }

}

#' Set title of an abf object, by-ref behaviour.
#'
#' In order to mimic the by-ref behaviour, when setting titles for a list of abf
#' objects, a temp variable "i_____" in caller's scope is used an then removed.
#' This may cause some trouble if you use i_____ in your code.
#'
#' @param abf an abf object, a list of abf objects are also supported.
#' @param title the title to be set.
#'
#' @return an abf object
#' @export
#'
#' @examples
#' #abf itself is changed, no need to assign
#' SetTitle(abf, "new title")
SetTitle <- function(abf, title) {

  if (class(abf) == "abf") {
    eval.parent(substitute({
      attr(abf, "title") <- as.character(title)
      invisible(abf)
    }))
  } else if (IsAbfList(abf)) {
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
    err_class_abf_list("SetTitle")
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
  ret <- meta$Protocol$lEpisodesPerRun

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
  ret <- meta$Protocol$lNumSamplesPerEpisode %/% GetNumOfChannel(abf)

  return(ret)
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