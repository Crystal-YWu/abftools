#' Get voltage channel id.
#'
#' @param abf an abf object or a list of abf objects.
#'
#' @return a vector of voltage channel id.
#' @export
#'
GetVoltageChan <- function(abf) {

  if (IsAbf(abf)) {
    ans <- which(GetChannelDesc(abf) == "Voltage")
    if (!length(ans)) {
      err_id_voltage_chan()
    }
  } else if (IsAbfList(abf)) {
    ans <- unique(lapply(abf, GetVoltageChan))
    if (length(ans) > 1L) {
      err_channel_config(abf)
    } else {
      ans <- unlist(ans)
    }
  } else {
    err_class_abf()
  }

  ans
}

#' Get current channel id.
#'
#' @param abf an abf object or a list of abf objects.
#'
#' @return a vector of current channel id.
#' @export
#'
GetCurrentChan <- function(abf) {

  if (IsAbf(abf)) {
    ans <- which(GetChannelDesc(abf) == "Current")
    if (!length(ans)) {
      err_id_voltage_chan()
    }
  } else if (IsAbfList(abf)) {
    ans <- unique(lapply(abf, GetCurrentChan))
    if (length(ans) > 1L) {
      err_channel_config(abf)
    } else {
      ans <- unlist(ans)
    }
  } else {
    err_class_abf()
  }

  ans
}

#' Get first voltage channel id.
#'
#' @param abf an abf object or a list of abf objects.
#'
#' @return an integer id of first voltage channel.
#' @export
#'
GetFirstVoltageChan <- function(abf) {

  FirstElement(GetVoltageChan(abf))
}

#' Get first current channel id.
#'
#' @param abf an abf object or a list of abf objects.
#'
#' @return an integer id of first current channel.
#' @export
#'
GetFirstCurrentChan <- function(abf) {

  FirstElement(GetCurrentChan(abf))
}

#' Return all channels of an abf object.
#'
#' @param abf an abf object.
#'
#' @return a vector of channel ids.
#' @export
#'
GetAllChannels <- function(abf) {

  if (!IsAbf(abf)) {
    err_class_abf()
  }

  return(seq_len(nChan(abf)))
}

CheckChannelDim <- function(abf, channel_data) {

  d1 <- dim(abf)
  d2 <- dim(channel_data)

  return(all(d1[1:2] == d2))
}

#' Attach a new channel to an abf object.
#'
#' The attached channel_data's dimensions must match original dimensions,
#' regardless of removed episodes due to the mechanism of RemoveEpisodes (they
#' are only marked removed and excluded when extracting using [[).
#'
#' @param abf an abf object.
#' @param channel_data data of the new channel.
#' @param channel_name name of the new channel.
#' @param channel_unit unit of the new channel.
#' @param channel_desc description of the new channel.
#'
#' @return an abf object with the attached channel.
#' @export
#'
AtchChan <- function(abf, channel_data,
                     channel_name, channel_unit, channel_desc = channel_name) {

  if (!IsAbf(abf)) {
    err_class_abf()
  }
  if (!CheckChannelDim(abf, channel_data)) {
    err_wrong_dim()
  }

  #new dimension
  d <- dim(abf)
  d[3] <- d[3] + 1L
  new_abf <- array(NA, dim = d)

  #copy to data
  nchan_old <- nChan(abf)
  nchan_new <- nchan_old + 1L
  new_abf[, , 1:nchan_old] <- abf
  new_abf[, , nchan_new] <- channel_data

  #copy meta information
  meta <- get_meta(abf)

  #set strings and corresponding ADC channel settings
  str <- meta$Strings
  str <- c(str, channel_name)
  channel_name_idx <- length(str)
  str <- c(str, channel_unit)
  channel_unit_idx <- length(str)
  meta$Strings <- str
  newadc <- dummy_adc_entry(nADCNum = nchan_new - 1L,
                            lADCChannelNameIndex = channel_name_idx,
                            lADCUnitsIndex = channel_unit_idx)
  meta$ADC <- rbind(meta$ADC, newadc)

  if (GetMode(abf) != 3L) {
    unit_lLength <- meta$SynchArray$lLength %/% nchan_old
    meta$SynchArray$lLength <- unit_lLength * nchan_new
  }

  #we should be good to go
  attr(new_abf, "class") <- "abf"
  attr(new_abf, "title") <- GetTitle(abf)
  attr(new_abf, "mode") <- GetMode(abf)

  attr(new_abf, "ChannelName") <- c(GetChannelName(abf), channel_name)
  attr(new_abf, "ChannelUnit") <- c(GetChannelUnit(abf), channel_unit)
  attr(new_abf, "ChannelDesc") <- c(GetChannelDesc(abf), channel_desc)
  attr(new_abf, "SamplingInterval") <- GetSamplingIntv(abf)
  attr(new_abf, "EpiAvail") <- attr(abf, "EpiAvail")

  attr(new_abf, "meta") <- meta

  return(new_abf)
}

#' Attach a new channel to an abf object, by-ref like behaviour.
#'
#' @param abf an abf object.
#' @param channel_data data of the new channel.
#' @param channel_name name of the new channel.
#' @param channel_unit unit of the new channel.
#' @param channel_desc description of the new channel.
#'
#' @return an abf object with the attached channel.
#' @export
#'
AttachChannel <- function(abf, channel_data, channel_name, channel_unit, channel_desc) {

  return(
    eval.parent(substitute({
      abf <- AtchChan(abf, channel_data, channel_name, channel_unit, channel_desc)
      invisible(abf)
    }))
  )
}

#' Replacing channel data.
#'
#' @param abf an abf object.
#' @param channel ADC channel id, 1-based.
#' @param channel_data channel data to replace the original.
#'
#' @return an abf object with the replaced channel.
#' @export
#'
RplcChan <- function(abf, channel, channel_data) {

  if (!IsAbf(abf)) {
    err_class_abf()
  }
  if (!AssertChannel(abf, channel)) {
    err_channel()
  }
  if (!CheckChannelDim(abf, channel_data)) {
    err_wrong_dim()
  }

  abf[, , channel] <- channel_data

  return(abf)
}

#' Replacing channel data, by-ref like behaviour.
#'
#' @param abf an abf object.
#' @param channel ADC channel id, 1-based.
#' @param channel_data channel data to replace the original.
#'
#' @return an abf object with the replaced channel.
#' @export
#'
ReplaceChannel <- function(abf, channel, channel_data) {

  return(
    eval.parent(substitute({
      abf <- RplcChan(abf, channel, channel_data)
      invisible(abf)
    }))
  )
}
