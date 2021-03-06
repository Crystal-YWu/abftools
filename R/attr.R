#copy channel related attr
ApplyChannelAttr <- function(x, abf, channel = NULL) {

  if (is.null(channel)) {
    attr(x, "ChannelName") <- GetChannelName(abf)
    attr(x, "ChannelUnit") <- GetChannelUnit(abf)
    attr(x, "ChannelDesc") <- GetChannelDesc(abf)
  } else {
    attr(x, "ChannelName") <- GetChannelName(abf)[channel]
    attr(x, "ChannelUnit") <- GetChannelUnit(abf)[channel]
    attr(x, "ChannelDesc") <- GetChannelDesc(abf)[channel]
  }

  x
}

ApplyAbfAttr <- function(x, abf) {

  structure(x,
            class = "abf", title = GetTitle(abf), mode = GetMode(abf),
            ChannelName = GetChannelName(abf),
            ChannelUnit = GetChannelUnit(abf),
            ChannelDesc = GetChannelDesc(abf),
            SamplingInterval = GetSamplingIntv(abf),
            EpiAvail = GetEpiAvail(abf),
            meta = get_meta(abf))
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

#' Get names of channels.
#'
#' @param abf an abf object.
#' @param channel channel id.
#'
#' @return a character vector of channel names.
#' @export
#'
GetChannelName <- function(abf, channel = NULL) {

  CheckArgs(abf, chan = channel, allow_list = TRUE)

  if (IsAbf(abf)) {
    if (is.null(channel)) {
      attr(abf, "ChannelName")
    } else {
      attr(abf, "ChannelName")[channel]
    }
  } else {
    if (is.null(channel)) {
      lapply(abf, function(x) attr(x, "ChannelName"))
    } else {
      lapply(abf, function(x) attr(x, "ChannelName")[channel])
    }
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

  CheckArgs(abf, chan = channel, allow_list = TRUE)

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

  CheckArgs(abf, chan = channel, allow_list = TRUE)

  if (IsAbf(abf)) {
    if (is.null(channel)) {
      attr(abf, "ChannelUnit")
    } else {
      attr(abf, "ChannelUnit")[channel]
    }
  } else {
    if (is.null(channel)) {
      lapply(abf, function(x) attr(x, "ChannelUnit"))
    } else {
      lapply(abf, function(x) attr(x, "ChannelUnit")[channel])
    }
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

  CheckArgs(abf, chan = channel, allow_list = TRUE)

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

  CheckArgs(abf, chan = channel, allow_list = TRUE)

  if (IsAbf(abf)) {
    if (is.null(channel)) {
      attr(abf, "ChannelDesc")
    } else {
      attr(abf, "ChannelDesc")[channel]
    }
  } else {
    if (is.null(channel)) {
      lapply(abf, function(x) attr(x, "ChannelDesc"))
    } else {
      lapply(abf, function(x) attr(x, "ChannelDesc")[channel])
    }
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

  CheckArgs(abf, chan = channel, allow_list = TRUE)

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
    lapply(abf, function(x) attr(x, "SamplingInterval"))
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

  f <- function(abf) get_meta(abf)$SynchArray

  if (IsAbf(abf)) {
    f(abf)
  } else if (IsAbfList(abf)) {
    lapply(abf, f)
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
    lapply(abf, function(x) dim(x)[3])
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
    lapply(abf, function(x) dim(x)[2])
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
    lapply(abf, function(x) dim(x)[1])
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

  f <- function(abf) {
    mode <- GetMode(abf)
    if (mode != 1L) {
      GetPointsPerEpisode(abf)
    } else {
      sync <- GetSynchArray(abf)
      sync$lLength[event] %/% GetNumOfChannel(abf)
    }
  }

  if (IsAbf(abf)) {
    f(abf)
  } else if (IsAbfList(abf)) {
    lapply(abf, f)
  } else {
    err_class_abf()
  }
}

#' Get number of epochs.
#'
#' @param abf an abf object
#' @param dac id of DAC channel
#'
#' @return number of epochs.
#' @export
#'
GetNumOfEpoch <- function(abf, dac = 1L) {

  f <- function(x) {
    epdac <- GetEpdac(x, dac)
    sapply(dac, function(d) sum(epdac$nDACNum == (d - 1L)))
  }

  if (IsAbf(abf)) {
    f(abf)
  } else if (IsAbfList(abf)) {
    lapply(abf, f)
  } else {
    err_class_abf()
  }
}

#' Get number of DAC.
#'
#' @param abf
#'
#' @return a vector of integer, number of DAC
#' @export
#'
GetNumOfDAC <- function(abf) {

  f <- function(x) {
    meta <- get_meta(x)
    length(meta$DAC$nDACNum)
  }

  if (IsAbf(abf)) {
    f(abf)
  } else if (IsAbfList(abf)) {
    lapply(abf, f)
  } else {
    err_class_abf()
  }
}

#' Get DAC id of which waveform is enabled.
#'
#' @param abf an abf object.
#'
#' @return DAC id, 1-based.
#' @export
#'
GetWaveformEnabledDAC <- function(abf) {

  f <- function(x) {
    meta <- get_meta(x)
    idx <- as.logical(meta$DAC$nWaveformEnable)
    meta$DAC$nDACNum[idx] + 1L
  }

  if (IsAbf(abf)) {
    f(abf)
  } else if (IsAbfList(abf)) {
    lapply(abf, f)
  } else {
    err_class_abf()
  }
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
nEpoch <- function(abf, dac = 1L) {

  GetNumOfEpoch(abf, dac)
}

#' @rdname GetNumOfDAC
#' @export
#'
nDAC <- function(abf) {

  GetNumOfDAC(abf)
}

nPtsHolding <- function(abf) {

  npts <- nPts(abf)
  npts %/% 64L
}
