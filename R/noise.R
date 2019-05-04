#' Denoise an abf object by applying wavelet shrink
#'
#' @param abf an abf object.
#' @param episode episode to denoise.
#' @param channel channel to denoise.
#' @param by_epoch wheter to denoise by epoch.
#' @param dac DAC channel that defines epochs.
#' @param thresh.scale controls strength of denoising. See \code{\link[wmtsa:wavShrink]{wavShrink()}} for details.
#' @param xform waveform transform type. See \code{\link[wmtsa:wavShrink]{wavShrink()}} for details.
#' @param ... passed to wavShrink()
#'
#' @return an abf object.
#' @export
#'
DnAbf <- function(abf, episode = GetAllEpisodes(abf), channel = 1L,
                  by_epoch = TRUE, dac = GetWaveformEnabledDAC(abf),
                  thresh.scale = 1.0, xform = "dwt", ...) {

  if (by_epoch) {
    CheckArgs(abf, epi = episode, chan = channel, dac = dac)

    epoch <- GetEpochIntervals(abf, dac = dac)
    nepo <- nEpoch(abf, dac)

    for (ch in channel) {
      for (epi in episode) {
        for (epo in seq_len(nepo)) {
          idx <- MaskIntv(epoch[, epi, epo])
          y <- abf[idx, epi, ch]
          abf[idx, epi, ch] <- wmtsa::wavShrink(y, thresh.scale = thresh.scale, xform = xform, ...)
        }
      }
    }

  } else {
    CheckArgs(abf, epi = episode, chan = channel)

    for (ch in channel) {
      for (epi in episode) {
        y <- abf[, epi, ch]
        abf[, epi, ch] <- wmtsa::wavShrink(y, thresh.scale = thresh.scale, xform = xform, ...)
      }
    }
  }

  abf
}

#' @rdname DnAbf
#' @export
#'
DenoiseAbf <- function(abf, episode, channel = 1L, by_epoch = TRUE, dac,
                       thresh.scale = 1.0, xform = "dwt", ...) {

  if (IsAbf(abf)) {
    eval.parent(substitute({
      abf <- DnAbf(abf, episode = episode, channel = channel, by_epoch = by_epoch,
                   dac = dac, thresh.scale = thresh.scale, xform = xform, ...)
      abf
    }))
  } else if (IsAbfList(abf)) {
    eval.parent(substitute({
      abf <- lapply(abf, DnAbf, episode = episode, channel = channel, by_epoch = by_epoch,
                    dac = dac, thresh.scale = thresh.scale, xform = xform, ...)
      abf
    }))
  } else {
    err_class_abf()
  }
}

#' Apply Butterworth low-pass filter to an abf object.
#'
#' @param abf an abf object.
#' @param channel the channel to apply filter.
#' @param freq low-pass frequency.
#' @param order filter order.
#'
#' @return an abf object.
#' @export
#'
LpAbf <- function(abf, channel, freq = 75, order = 1L) {

  CheckArgs(abf, chan = channel)

  ApplyLowpass(abf, chan = channel, freq = freq, order = order)
}

#' @rdname LpAbf
#' @export
#'
LowpassAbf <- function(abf, channel, freq = 75, order = 1L) {

  if (IsAbf(abf)) {
    eval.parent(substitute({
      abf <- LpAbf(abf, channel = channel, freq = freq, order = order)
      abf
    }))
  } else if (IsAbfList(abf)) {
    eval.parent(substitute({
      abf <- lapply(abf, LpAbf, channel = channel, freq = freq, order = order)
      abf
    }))
  } else {
    err_class_abf()
  }
}

ApplyLowpass <- function(abf, chan, freq, order) {

  bf <- signal::butter(n = order, W = 1 / (2*freq), type = "low")
  ff <- function(x) signal::filter(bf, x)
  for (ch in chan) {

    abf[,, ch] <- mapnd(abf[,, ch], ff, along = 1L)
  }

  abf
}
