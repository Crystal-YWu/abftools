GetFirstVoltageChan <- function(abf) match("Voltage", GetChannelDesc(abf))
GetFirstCurrentChan <- function(abf) match("Current", GetChannelDesc(abf))

#' Title
#'
#' @param abf
#' @param new_channel
#' @param channel_name
#' @param channel_unit
#' @param channel_desc
#'
#' @return
#' @export
#'
#' @examples
AtchChan <- function(abf, new_channel, channel_name, channel_unit, channel_desc) {

  if (class(abf) != "abf") {
    err_class_abf("AtchChan")
  }

  #old dimension
  d <- dim(abf)
  dchan <- dim(new_channel)
  if ((length(dchan) != 2L) || (!all(dchan == d[1:2]))) {
    err_wrong_dim("AttachChannel")
  }
  #new dimension
  d[3] <- d[3] + 1L
  new_abf <- array(NA, dim = d)

  #copy to data
  nchan_old <- nChan(abf)
  nchan_new <- nchan_old + 1L
  new_abf[, , 1:nchan_old] <- abf
  new_abf[, , nchan_new] <- new_channel

  #copy meta information
  meta <- get_meta(abf)

  #craft an ADC record that make sense
  same_unit <- match(channel_unit, GetChannelUnit(abf))
  if (!is.na(same_unit)) {
    #we've found an ADC channel that has same unit, so we can pretend that the new
    #channel has same settings
    meta$ADC <- rbind(meta$ADC, meta$ADC[same_unit, ])
  } else {
    #new channel has a new unit, so we need to craft arbitrary ADC settings for it
    meta$ADC <- rbind(meta$ADC, meta$ADC[nchan_old, ])
    #new channel does not offset, and only scales to fADCRange/lADCResolution
    meta$ADC$fSignalGain[nchan_new] <- 1.0
    meta$ADC$fADCProgrammableGain[nchan_new] <- 1.0
    meta$ADC$fTelegraphAdditGain <- 1.0
    meta$ADC$fInstrumentOffset <- 0.0
    meta$ADC$fSignalOffset <- 0.0
  }
  #set correct ADC num
  meta$ADC$nADCNum <- nchan_new - 1L

  #set strings and corresponding ADC channel settings
  str <- meta$Strings
  str <- c(str, channel_name)
  channel_name_idx <- length(str)
  str <- c(str, channel_unit)
  channel_unit_idx <- length(str)
  meta$Strings <- str
  meta$ADC$lADCChannelNameIndex[nchan_new] <- channel_name_idx
  meta$ADC$lADCUnitsIndex[nchan_new] <- channel_unit_idx

  #set lNumSamplesPerEpisode
  npts <- nPts(abf)
  meta$Protocol$lNumSamplesPerEpisode <- npts * nchan_new

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

#' Title
#'
#' @param abf
#' @param new_channel
#' @param channel_name
#' @param channel_unit
#' @param channel_desc
#'
#' @return
#' @export
#'
#' @examples
AttachChannel <- function(abf, new_channel, channel_name, channel_unit, channel_desc) {

  return(
    eval.parent(substitute({
      abf <- AtchChan(abf, new_channel, channel_name, channel_unit, channel_desc)
    }))
  )
}

#' Title
#'
#' @param abf
#' @param channel
#' @param channel_data
#'
#' @return
#' @export
#'
#' @examples
RplcChan <- function(abf, channel, channel_data) {

  if (class(abf) != "abf") {
    err_class_abf("RpclChan")
  }

  abf[, , channel] <- channel_data

  return(abf)
}

#' Title
#'
#' @param abf
#' @param channel
#' @param channel_data
#'
#' @return
#' @export
#'
#' @examples
ReplaceChannel <- function(abf, channel, channel_data) {

  return(
    eval.parent(substitute({
      abf <- RplcChan(abf, channel, channel_data)
    }))
  )
}
