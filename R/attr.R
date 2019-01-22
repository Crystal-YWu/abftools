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

  attr(abf, "ChannelName")
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

  attr(abf, "ChannelUnit")
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

  attr(abf, "ChannelDesc")
}

#' Set channel unit.
#'
#' @param abf an abf object.
#' @param unit a new unit
#' @param channel channel id.
#'
#' @return a abf object with new unit set.
#' @export
#'
SetChanUnit <- function(abf, unit, channel = 1L) {

  if (IsAbf(abf)) {

    units <- GetChannelUnit(abf)
    units[channel] <- as.character(unit)
    attr(abf, "ChannelUnit") <- units

  } else {

    err_class_abf()
  }

  abf
}

#' Set channel description.
#'
#' @param abf an abf object.
#' @param desc a new description for the channel.
#' @param channel channel id.
#'
#' @return a abf object with new ChannelDesc set.
#' @export
#'
SetChanDesc <- function(abf, desc, channel = 1L) {

  if (IsAbf(abf)) {

    descs <- GetChannelDesc(abf)
    descs[channel] <- as.character(desc)
    attr(abf, "ChannelDesc") <- descs

  } else {

    err_class_abf()
  }

  abf
}

#' Set channel unit, by-ref behaviour.
#'
#' @param abf an abf object.
#' @param unit a new unit
#' @param channel channel id.
#'
#' @return a abf object with new unit set.
#' @export
#'
SetChannelUnit <- function(abf, unit, channel = 1L) {

  if (IsAbf(abf)) {

    eval.parent(substitute({
      abf <- SetChanUnit(abf, unit, channel)
      invisible(abf)
    }))

  } else if (IsAbfList(abf)) {

    eval.parent(substitute({
      abf <- lapply(abf, function(x) SetChanUnit(x, unit, channel))
      invisible(abf)
    }))

  } else {
    err_class_abf_list()
  }
}

#' Set channel description, by-ref behaviour.
#'
#' @param abf an abf object.
#' @param description a new description for the channel.
#' @param channel channel id.
#'
#' @return a abf object with new ChannelDesc set.
#' @export
#'
SetChannelDesc <- function(abf, description, channel = 1L) {

  if (IsAbf(abf)) {

    eval.parent(substitute({
      abf <- SetChanDesc(abf, description, channel)
      invisible(abf)
    }))

  } else if (IsAbfList(abf)) {

    eval.parent(substitute({
      abf <- lapply(abf, function(x) SetChanDesc(x, description, channel))
      invisible(abf)
    }))

  } else {
    err_class_abf_list()
  }
}

CpChannelAttr <- function(x, abf, channel = NULL) {

  if (is.null(channel)) {
    name <- GetChannelName(abf)
    unit <- GetChannelUnit(abf)
    desc <- GetChannelDesc(abf)
  } else {
    name <- GetChannelName(abf)[channel]
    unit <- GetChannelUnit(abf)[channel]
    desc <- GetChannelDesc(abf)[channel]
  }

  attr(x, "ChannelName") <- name
  attr(x, "ChannelUnit") <- unit
  attr(x, "ChannelDesc") <- desc

  x
}

CpAbfAttr <- function(x, abf, cp_class = TRUE) {

  attr(x, "title") <- GetTitle(abf)
  attr(x, "mode") <- GetMode(abf)

  x <- CpChannelAttr(x, abf)

  attr(x, "SamplingInterval") <- GetSamplingIntv(abf)
  attr(x, "EpiAvail") <- attr(abf, "EpiAvail")
  if (cp_class) {
    attr(x, "class") <- "abf"
  }

  x
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

  attr(abf, "SamplingInterval")
}

#' @rdname GetSamplingIntv
#' @export
#'
GetSamplingRate <- function(abf) {

  GetSamplingIntv(abf)
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

  attr(abf, "mode")
}

#' Get number of channels.
#'
#' @param abf an abf object.
#'
#' @return number of channels.
#' @export
#'
GetNumOfChannel <- function(abf) {

  dim(abf)[3]
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

  dim(abf)[2]
}

#' Get number of recorded points per episode/sweep.
#'
#' @param abf an abf object.
#'
#' @return number of recorded points per episode/sweep.
#' @export
#'
GetPointsPerEpisode <- function(abf, event = NULL) {

  dim(abf)[1]
}

#' Get number of recorded points for corresponding event.
#'
#' @param abf an abf object.
#' @param event id of event.
#'
#' @return number of recorded points.
#' @export
#'
GetPointsPerEvent <- function(abf, event = 1L) {

  mode <- GetMode(abf)
  if (mode != 1L) {
    ans <- GetPointsPerEpisode(abf)
  } else {
    meta <- get_meta(abf)
    ans <- meta$SynchArray$lLength[event] %/% nChan(abf)
  }

  ans
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

  nrow(epdac)
}

#' @rdname GetNumOfChannel
#' @export
#'
nChan <- function(abf) {

  GetNumOfChannel(abf)
}

#' @rdname GetPointsPerEpisode
#' @export
#'
nPts <- function(abf) {

  GetPointsPerEpisode(abf)
}

#' @rdname GetPointsPerEvent
#' @export
#'
nPtsEvent <- function(abf, event = 1L) {

  GetPointsPerEvent(abf, event)
}

#' @rdname GetEpisodesPerChannel
#' @export
#'
nEpi <- function(abf) {

  GetEpisodesPerChannel(abf)
}

#' @rdname GetNumOfEpoch
#' @export
#'
nEpoch <- function(abf) {

  GetNumOfEpoch(abf)
}
