#' Denoise an abf object by epoch
#'
#' Currently available algorithms are: wavshrink (hard threshold), sureshrink
#' (soft threshold)
#'
#' @param abf an abf object.
#' @param epoch epoch name/id.
#' @param episodes episodes/sweeps to denoise.
#' @param channel channel id, 1-based.
#' @param algo algorithm to denoise.
#' @param ... other arguments to pass to the selected algorithm.
#'
#' @return a named list of denoised episodes/sweeps.
#' @export
#'
DenoiseEpoch <- function(abf, epoch, episodes, channel = 1,
                         algo = "sureshrink", ...) {

  if (!IsAbf(abf)) {
    err_class_abf()
  }
  epoch <- FirstElement(epoch)
  if (is.character(epoch)) {
    epoch <- GetEpochId(epoch)
  }
  if (!AssertEpoch(abf, epoch)) {
    err_epoch()
  }
  if (missing(episodes) || is.null(episodes)) {
    episodes <- seq_len(nEpi(abf))
  } else if (!AssertEpisode(abf, episodes)) {
    err_epi()
  }
  if (!AssertChannel(abf, channel)) {
    err_channel()
  }
  denoised <- ExternalAlgoEpoch(abf, epoch, episodes, channel,
                                "denoise", algo, ...)

  return(denoised)
}

#' Denoise an abf object by an interval
#'
#' Currently available algorithms are: wavshrink (hard threshold), sureshrink
#' (soft threshold)
#'
#' @param abf an abf object.
#' @param intv an interval to denoise.
#' @param episodes episodes/sweeps to denoise.
#' @param channel channel id, 1-based.
#' @param algo algorithm to denoise.
#' @param ... other arguments to pass to the selected algorithm.
#'
#' @return a named list of denoised episodes/sweeps.
#' @export
#'
DenoiseIntv <- function(abf, intv, episodes, channel = 1,
                        algo = "sureshrink", ...) {

  if (!IsAbf(abf)) {
    err_class_abf()
  }
  if (missing(episodes) || is.null(episodes)) {
    episodes <- seq_len(nEpi(abf))
  } else if (!AssertEpisode(abf, episodes)) {
    err_epi()
  }
  if (!AssertChannel(abf, channel)) {
    err_channel()
  }
  denoised <- ExternalAlgoIntv(abf, intv, episodes, channel,
                               "denoise", algo, ...)

  return(denoised)
}

#hard thresholding
denoise_wavshrink <- function(y, smoothness = 1, ...) {

  z <- wmtsa::wavShrink(y, thresh.fun = "universal", thresh.scale = smoothness, ...)
  return(z)
}

#soft thresholding
denoise_sureshrink <- function(y, smoothness = 1, ...) {

  z <- wmtsa::wavShrink(y, thresh.fun = "adaptive", thresh.scale = smoothness, ...)
  return(z)
}
