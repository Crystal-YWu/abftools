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
DenoiseAbf <- function(abf, episode = GetAllEpisodes(abf), channel = 1L,
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

denoise_wavshrink <- function(y, thresh.scale = 1.0, xform = "dwt", ...) {

  wmtsa::wavShrink(y, thresh.scale = thresh.scale, xform = xform, ...)
}
