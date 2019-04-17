GetYLimit <- function(abf, intv = NULL, curs = NULL, channel, blank = 0.0125) {

  #convert abf, intv, curs to lists.
  if (IsAbf(abf)) {
    abf <- list(abf)
  }
  #pad intv and curs
  if (!is.list(intv)) {
    intv <- list(intv)
  }
  if (!is.list(curs)) {
    curs <- list(curs)
  }

  ret <- NULL
  for (i in seq_along(abf)) {

    #get pts of interest
    poi <- c(intv[[i]][1], intv[[i]][2], curs[[i]])
    if (is.null(poi)) {
      #no intv or curs is given, set mask to whole channel
      mask <- seq_len(nPts(abf[[i]]))
    } else {
      t_range <- range(poi)
      mask <- seq(t_range[1], t_range[2])
    }

    lower <- min(abf[[i]][[channel]][mask, ])
    upper <- max(abf[[i]][[channel]][mask, ])
    delta <- abs(lower - upper)
    lower <- lower - delta * blank
    upper <- upper + delta * blank

    ret <- range(ret, upper, lower)
  }

  ret
}

#' Calculate time span of a frequency
#'
#' @param abf an abf object.
#' @param freq a frequency in Hz.
#'
#' @return an integer of **point**/**tick** count.
#' @export
#'
FreqToTick <- function(abf, freq) {

  time <- 1e6 / freq
  tick <- time / GetSamplingRate(abf)

  ceiling(tick)
}

#' Convert tick (array index) to time unit.
#'
#' @param abf an abf object.
#' @param tick a vector of integer.
#' @param time_unit desired time unit.
#'
#' @return a vector of numeric.
#' @export
#'
TickToTime <- function(abf, tick, time_unit = c("tick", "us", "ms", "s", "min", "hr")) {

  time_unit <- match.arg(time_unit)
  time <- switch(time_unit,
                 tick = tick,
                 us =  (tick - 1L) * GetSamplingIntv(abf),
                 ms =  (tick - 1L) * GetSamplingIntv(abf) / 1000,
                 s  =  (tick - 1L) * GetSamplingIntv(abf) / 1000 / 1000,
                 min = (tick - 1L) * GetSamplingIntv(abf) / 1000 / 1000 / 60,
                 hr =  (tick - 1L) * GetSamplingIntv(abf) / 1000 / 1000 / 60 / 60)

  time
}


#####################################

GetAxisLabel <- function(desc, unit, style) sprintf(style, desc, unit)

#' Compose labels for every episode/channel of an abf object
#'
#' @param abf an abf object
#' @param style a format string
#'
#' @return a vector of characters.
#' @export
#'
GetEpiLabel <- function(abf, style = "epi%d") {

  if (IsAbf(abf)) {
    nep <- nEpi(abf)
  } else if (is.numeric(abf)) {
    nep <- as.integer(abf)
  } else {
    err_class_abf()
  }

  if (length(nep) == 1L) {
    sprintf(style, seq_len(nep))
  } else {
    sprintf(style, nep)
  }
}

#' @rdname GetEpiLabel
#' @export
#'
GetChanLabel <- function(abf, style = "%s (%s)") {

  if (!IsAbf(abf)) {
    err_class_abf()
  }

  GetAxisLabel(GetChannelDesc(abf), GetChannelUnit(abf), style = style)
}

#' @rdname GetEpiLabel
#' @export
#'
DefaultEpiLabel <- function(abf) {

  GetEpiLabel(abf, style = "epi%d")
}

#' @rdname GetEpiLabel
#' @export
#'
DefaultChanLabel <- function(abf) {

  GetChanLabel(abf, style = "%s (%s)")
}

#' Compose default episode/channel number tag.
#'
#' @param episode episode numbers
#' @param channel channel numbers
#'
#' @return a vector of characters
#' @export
#'
#' @examples
#' epi_tag <- GetEpiTag(c(1,3,4,5))
#' ch_tag <- GetChanTag(2:3)
GetEpiTag <- function(episode) {

  if (IsAbf(episode)) {
    episode <- GetAllEpisodes(episode)
  }

  sprintf("epi%d", episode)
}

#' @rdname GetEpiTag
#' @export
#'
GetChanTag <- function(channel) {

  if (IsAbf(channel)) {
    channel <- GetAllChannels(channel)
  }

  sprintf("chan%d", channel)
}
