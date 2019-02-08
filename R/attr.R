#define attr of an abf object
ApplyAbfAttr <- function(x, class = "abf", title, mode,
                         ChannelName, ChannelUnit, ChannelDesc,
                         SamplingInterval, EpiAvail, meta = NULL) {

  eval.parent(substitute({
    attr(x, "class") <- class
    attr(x, "title") <- title
    attr(x, "mode") <- mode

    attr(x, "ChannelName") <- ChannelName
    attr(x, "ChannelUnit") <- ChannelUnit
    attr(x, "ChannelDesc") <- ChannelDesc

    attr(x, "SamplingInterval") <- SamplingInterval
    attr(x, "EpiAvail") <- EpiAvail

    if (!is.null(meta)) {
      attr(x, "meta") <- meta
    }

    invisible(x)
  }))
}

#copy channel related attr
CpChannelAttr <- function(x, abf, channel = NULL) {

  eval.parent(substitute({
    if (is.null(channel)) {
      attr(x, "ChannelName") <- GetChannelName(abf)
      attr(x, "ChannelUnit") <- GetChannelUnit(abf)
      attr(x, "ChannelDesc") <- GetChannelDesc(abf)
    } else {
      attr(x, "ChannelName") <- GetChannelName(abf)[channel]
      attr(x, "ChannelUnit") <- GetChannelUnit(abf)[channel]
      attr(x, "ChannelDesc") <- GetChannelDesc(abf)[channel]
    }
    invisible(x)
  }))
}

CpAbfAttr <- function(x, abf) {

  eval.parent(substitute({
    ApplyAbfAttr(x, title = GetTitle(abf), mode = GetMode(abf),
                 ChannelName = GetChannelName(abf),
                 ChannelUnit = GetChannelUnit(abf),
                 ChannelDesc = GetChannelDesc(abf),
                 SamplingInterval = GetSamplingIntv(abf),
                 EpiAvail = GetEpiAvail(abf),
                 meta = get_meta(abf))
    invisible(x)
  }))
}

#' Get title of an abf object.
#'
#' @param abf an abf object.
#'
#' @return the title of the abf object.
#' @export
#'
GetTitle <- function(abf) {

  if (IsAbf(abf)) {
    attr(abf, "title")
  } else if (IsAbfList(abf)) {
    lapply(abf, function(x) attr(x, "title"))
  } else {
    err_class_abf_list()
  }
}

#' Set title of an abf object, by-ref behaviour.
#'
#'
#' @param abf an abf object, a list of abf objects are also supported.
#' @param title the title to be set.
#'
#' @return an abf object, invisibly
#' @export
#'
SetTitle <- function(abf, title) {

  if (IsAbf(abf)) {
    eval.parent(substitute({
      attr(abf, "title") <- as.character(title)
      invisible(abf)
    }))
  } else if (IsAbfList(abf)) {
      eval.parent(substitute({
        abf <- mapply(function(x, xtitle) {
          attr(x, "title") <- xtitle
          x
        }, x = abf, xtitle = title, SIMPLIFY = FALSE)
        invisible(abf)
      }))
  } else {
    err_class_abf_list()
  }
}

#' Get mode of the abf object.
#'
#' @param abf an abf object.
#'
#' @return the mode of the recording.
#' @export
#'
GetMode <- function(abf) {

  if (IsAbf(abf)) {
    attr(abf, "mode")
  } else if (IsAbfList(abf)) {
    lapply(abf, function(x) attr(x, "mode"))
  } else {
    err_class_abf()
  }
}

validate_channel <- function(abf, channel) {

  if (is.null(channel)) {
    return(TRUE)
  }

  if (IsAbf(abf)) {
    AssertChannel(abf, channel)
  } else if (IsAbfList(abf)) {
    all(sapply(abf, function(x) AssertChannel(x, channel)))
  } else {
    err_class_abf()
  }
}

#' Get names of channels.
#'
#' @param abf an abf object.
#' @param channel channel id.
#'
#' @return a character vector of channel names.
#' @export
#'
GetChannelName <- function(abf, channel = NULL) {

  if (!validate_channel(abf, channel)) {
    err_channel()
  }

  if (IsAbf(abf)) {
    if (is.null(channel)) {
      attr(abf, "ChannelName")
    } else {
      attr(abf, "ChannelName")[channel]
    }
  } else {
    lapply(abf, GetChannelName, channel = channel)
  }
}

#' Set channel name, by-ref behaviour.
#'
#' @param abf an abf object.
#' @param name a new name
#' @param channel channel id.
#'
#' @return a abf object with new name set, invisibly.
#' @export
#'
SetChannelName <- function(abf, name, channel = 1L) {

  if (!validate_channel(abf, channel)) {
    err_channel()
  }

  if (IsAbf(abf)) {
    eval.parent(substitute({
      attr(abf, "ChannelName")[channel] <- name
      invisible(abf)
    }))
  } else {
    eval.parent(substitute({
      abf <- lapply(abf, function(x) {
        attr(x, "ChannelName")[channel] <- name
        x
      })
      invisible(abf)
    }))
  }
}

#' Get units of channels.
#'
#' @param abf an abf object.
#' @param channel channel id.
#'
#' @return a character vector of channel units.
#' @export
#'
GetChannelUnit <- function(abf, channel = NULL) {

  if (!validate_channel(abf, channel)) {
    err_channel()
  }

  if (IsAbf(abf)) {
    if (is.null(channel)) {
      attr(abf, "ChannelUnit")
    } else {
      attr(abf, "ChannelUnit")[channel]
    }
  } else {
    lapply(abf, GetChannelUnit, channel = channel)
  }
}

#' Set channel unit, by-ref behaviour.
#'
#' @param abf an abf object.
#' @param unit a new unit
#' @param channel channel id.
#'
#' @return a abf object with new unit set, invisibly.
#' @export
#'
SetChannelUnit <- function(abf, unit, channel = 1L) {

  if (!validate_channel(abf, channel)) {
    err_channel()
  }

  if (IsAbf(abf)) {
    eval.parent(substitute({
      attr(abf, "ChannelUnit")[channel] <- unit
      invisible(abf)
    }))
  } else {
    eval.parent(substitute({
      abf <- lapply(abf, function(x) {
        attr(x, "ChannelUnit")[channel] <- unit
        x
      })
      invisible(abf)
    }))
  }
}

#' Get descriptions of channels.
#'
#' @param abf an abf object.
#'
#' @return a character vector of channel descriptions.
#' @export
#'
GetChannelDesc <- function(abf, channel = NULL) {

  if (!validate_channel(abf, channel)) {
    err_channel()
  }

  if (IsAbf(abf)) {
    if (is.null(channel)) {
      attr(abf, "ChannelDesc")
    } else {
      attr(abf, "ChannelDesc")[channel]
    }
  } else {
    lapply(abf, GetChannelDesc, channel = channel)
  }
}

#' Set channel description, by-ref behaviour.
#'
#' @param abf an abf object.
#' @param description a new description for the channel.
#' @param channel channel id.
#'
#' @return a abf object with new description set, invisibly.
#' @export
#'
SetChannelDesc <- function(abf, description, channel = 1L) {

  if (!validate_channel(abf, channel)) {
    err_channel()
  }

  if (IsAbf(abf)) {
    eval.parent(substitute({
      attr(abf, "ChannelDesc")[channel] <- description
      invisible(abf)
    }))
  } else {
    eval.parent(substitute({
      abf <- lapply(abf, function(x) {
        attr(x, "ChannelDesc")[channel] <- description
        x
      })
      invisible(abf)
    }))
  }
}

#' Get sampling interval in us.
#'
#' @param abf an abf object.
#'
#' @return the sampling interval in unit us.
#' @export
#'
GetSamplingIntv <- function(abf) {

  if (IsAbf(abf)) {
    attr(abf, "SamplingInterval")
  } else if (IsAbfList(abf)) {
    lapply(abf, GetSamplingIntv)
  } else {
    err_class_abf()
  }
}

#' @rdname GetSamplingIntv
#' @export
#'
GetSamplingRate <- function(abf) {

  GetSamplingIntv(abf)
}

GetEpiAvail <- function(abf) attr(abf, "EpiAvail")

#' Return sync array of an abf object.
#'
#' @param abf an abf object.
#'
#' @return a sync array
#' @export
#'
GetSynchArray <- function(abf) {

  if (IsAbf(abf)) {
    meta <- get_meta(abf)
    meta$SynchArray
  } else if (IsAbfList(abf)) {
    lapply(abf, GetSamplingIntv)
  } else {
    err_class_abf()
  }
}

#' Get number of channels.
#'
#' @param abf an abf object.
#'
#' @return number of channels.
#' @export
#'
GetNumOfChannel <- function(abf) {

  if (IsAbf(abf)) {
    dim(abf)[3]
  } else if (IsAbfList(abf)) {
    lapply(abf, GetNumOfChannel)
  } else {
    err_class_abf()
  }
}

#' Get number of episodes/sweeps per channel.
#'
#' @param abf an abf object.
#'
#' @return number of episodes/sweeps per channel.
#' @export
#'
GetEpisodesPerChannel <- function(abf) {

  if (IsAbf(abf)) {
    dim(abf)[2]
  } else if (IsAbfList(abf)) {
    lapply(abf, GetEpisodesPerChannel)
  } else {
    err_class_abf()
  }
}

#' Get number of recorded points per episode/sweep.
#'
#' @param abf an abf object.
#'
#' @return number of recorded points per episode/sweep.
#' @export
#'
GetPointsPerEpisode <- function(abf) {

  if (IsAbf(abf)) {
    dim(abf)[1]
  } else if (IsAbfList(abf)) {
    lapply(abf, GetPointsPerEpisode)
  } else {
    err_class_abf()
  }
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

  if (IsAbf(abf)) {
    mode <- GetMode(abf)
    if (mode != 1L) {
      ans <- GetPointsPerEpisode(abf)
    } else {
      sync <- GetSynchArray(abf)
      ans <- sync$lLength[event] %/% GetNumOfChannel(abf)
    }
  } else if (IsAbfList(abf)) {
    ans <- lapply(abf, GetPointsPerEvent, event = event)
  } else {
    err_class_abf()
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

  if (IsAbf(abf)) {
    meta <- get_meta(abf)
    epdac <- meta$EpochPerDAC
    if (is.null(epdac)) {
      ans <- 0L
    } else {
      ans <- nrow(epdac)
    }
  } else if (IsAbfList(abf)) {
    ans <- lapply(abf, GetNumOfEpoch)
  } else {
    err_class_abf()
  }

  ans
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
