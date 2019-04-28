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

  CheckArgs(abf)

  len1 <- length(abf[,, 1L])
  len2 <- length(channel_data)
  if (!len1 == len2) {
    eval(substitute(err_wrong_dim(abf, channel_data, esc_eval = TRUE)))
  }

  #new dimension
  d <- dim(abf)
  d[3] <- d[3] + 1L
  new_abf <- c(abf, channel_data)
  dim(new_abf) <- d

  #copy to data
  nchan_old <- nChan(abf)
  nchan_new <- nchan_old + 1L

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

  if (!is.null(meta$SynchArray)) {
    unit_lLength <- meta$SynchArray$lLength %/% nchan_old
    meta$SynchArray$lLength <- unit_lLength * nchan_new
  }

  #we should be good to go
  ApplyAbfAttr(new_abf, title = GetTitle(abf), mode = GetMode(abf),
               ChannelName = c(GetChannelName(abf), channel_name),
               ChannelUnit = c(GetChannelUnit(abf), channel_unit),
               ChannelDesc = c(GetChannelDesc(abf), channel_desc),
               SamplingInterval = GetSamplingIntv(abf),
               EpiAvail = GetEpiAvail(abf), meta = meta)
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

    eval.parent(substitute({
      abf <- AtchChan(abf, channel_data, channel_name, channel_unit, channel_desc)
      invisible(abf)
    }))
}

#' Replacing channel data.
#'
#' @param abf an abf object.
#' @param channel_data channel data to replace the original.
#' @param channel ADC channel id, 1-based.
#'
#' @return an abf object with the replaced channel.
#' @export
#'
RplcChan <- function(abf, channel_data, channel = 1L) {

  CheckArgs(abf, chan = channel)

  len1 <- length(abf[,, 1L])
  len2 <- length(channel_data)
  if (!len1 == len2) {
    eval(substitute(err_wrong_dim(abf, channel_data, esc_eval = TRUE)))
  }

  abf[, , channel] <- channel_data
  abf
}

#' Replacing channel data, by-ref like behaviour.
#'
#' @param abf an abf object.
#' @param channel_data channel data to replace the original.
#' @param channel ADC channel id, 1-based.
#'
#' @return an abf object with the replaced channel.
#' @export
#'
ReplaceChannel <- function(abf, channel_data, channel = 1L) {

    eval.parent(substitute({
      abf <- RplcChan(abf, channel_data, channel)
      invisible(abf)
    }))
}

AtchChan_unsafe <- function(abf, channel_data,
                            channel_name, channel_unit, channel_desc = channel_name) {

  d <- dim(abf)
  d[3] <- d[3] + 1L
  new_abf <- c(abf, channel_data)
  dim(new_abf) <- d

  ApplyAbfAttr(new_abf, title = GetTitle(abf), mode = GetMode(abf),
               ChannelName = c(GetChannelName(abf), channel_name),
               ChannelUnit = c(GetChannelUnit(abf), channel_unit),
               ChannelDesc = c(GetChannelDesc(abf), channel_desc),
               SamplingInterval = GetSamplingIntv(abf),
               EpiAvail = GetEpiAvail(abf), meta = NULL)
}

#' Rescale channel unit.
#'
#' @param abf an abf object
#' @param channel channel to rescale
#' @param scale scale to use
#'
#' @return abf itself
#' @export
#'
RescaleChannel <- function(abf, channel, scale = c("1", "pico", "nano", "micro", "milli",
                                                   "kilo", "mega", "Giga", "Tera")) {
  eval.parent(substitute({
    abf <- RsclChan(abf, channel, scale)
    invisible(abf)
  }))
}

#' @rdname RescaleChannel
#' @export
#'
RsclChan <- function(abf, channel = GetAllChannels(abf),
                     scale = c("1", "pico", "nano", "micro", "milli",
                               "kilo", "mega", "Giga", "Tera")) {

  CheckArgs(abf, chan = channel)
  scale <- as.character(scale)
  scale <- match.arg(scale)

  scale_factor <- function(unit, scale) {
    old_scale <- parse_unit_scale(unit)
    new_scale <- parse_unit_scale(scale)
    old_scale / new_scale
  }

  do_scale <- function(abf, channel, scale) {
    unit <- GetChannelUnit(abf)
    for (ch in channel) {
      abf[,, ch] <- abf[,, ch] * scale_factor(unit[ch], scale)
      long_prefix <- parse_unit_prefix(unit[ch])
      suffix <- parse_unit_suffix(unit[ch])
      if (long_prefix) {
        if (scale == "1") {
          prefix <- ""
        } else {
          prefix <- scale
        }
      } else {
        prefix <- switch(scale,
                         pico  = "p",
                         nano  = "n",
                         micro = "µ",
                         milli = "m",
                         kilo  = "k",
                         mega  = "M",
                         Giga  = "G",
                         Tera  = "T",
                         "")
      }
      new_unit <- paste0(prefix, suffix)
      SetChannelUnit(abf, new_unit, channel = ch)
    }
    abf
  }

  if (IsAbfList(abf)) {
    lapply(abf, do_scale, channel = channel, scale = scale)
  } else {
    do_scale(abf, channel, scale)
  }
}
