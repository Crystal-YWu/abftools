#' Average a list of abf objects.
#'
#' @param abf_list a list of abf objects.
#' @param w OPTIONAL, a vector of weights for weighted average.
#'
#' @return an averaged abf object, of which the protocol settings follow first element in abf_list.
#' @export
#'
AverageAbf <- function(abf_list, w = NULL) {

  if (!IsAbfList(abf_list)) {
    err_class_abf_list()
  }

  if (is.null(w)) {
    n <- length(abf_list)
    ret <- abf_list[[1]]
    for (i in 2:n) {
      ret <- ret + abf_list[[i]]
    }
    ret <- ret / n
  } else {
    if (!AssertLength(w, abf_list)) {
      err_assert_len(w, abf_list)
    }

    n <- length(abf_list)
    ret <- abf_list[[1]] * w[1]
    for (i in 2:n) {
      ret <- ret + abf_list[[i]] * w[i]
    }
    ret <- ret / sum(w)
  }


  ret
}

ApplyBlank <- function(abf, chan, epi_val) {

  abf[,, chan] <- sweep(abf[,, chan], 2L, epi_val)
  abf
}

#' Blank current channel of abf objects.
#'
#' @param abf an abf object or a list of abf objects.
#' @param ref_data reference current data to blank or abf objects as reference.
#' @param ref_intv OPTIONAL, if ref_data is abf objects, a time interval to
#' calculate mean current.
#' @param current_channel OPTIONAL, the current channel to apply blanking,
#' if not given, the first current channel is used.
#'
#'
#' @return an abf object or a list of abf objects
#' @export
#'
BlankAbf <- function(abf, ref_data, ref_intv = NULL,
                     current_channel = GetFirstCurrentChan(abf)) {

  #reference data
  if (IsAbf(ref_data)) {
    ref_data <- mean(ref_data, intv = ref_intv)
  } else if (IsAbfList(ref_data)) {
    ref_data <- IVSummary(ref_data, ref_intv, current_channel = current_channel)
  }
  current_data <- ParseDataFrameIV(ref_data)$Current
  idx_na <- is.na(current_data)
  if (all(idx_na)) {
    err_channel_data("Current")
  } else {
    current_data[which(idx_na)] <- 0.0
  }

  if (IsAbf(abf)) {
    abf <- ApplyBlank(abf, current_channel, current_data)
  } else if (IsAbfList(abf)) {
    abf <- lapply(abf, ApplyBlank, current_channel, current_data)
  }

  abf
}

ApplyLowpass <- function(abf, chan, freq, order) {

  bf <- signal::butter(order, 1 / (2*freq), "low")
  ff <- function(x) signal::filter(bf, x)
  for (ch in chan) {

    abf[,, ch] <- mapnd(abf[,, ch], ff, along = 1L)
  }

  abf
}

#' Apply low-pass filter to an abf object.
#'
#' @param abf an abf object.
#' @param channel the channel to apply filter.
#' @param freq low-pass frequency.
#' @param order filter order.
#'
#' @return an abf object.
#' @export
#'
LowpassAbf <- function(abf, channel, freq = 75, order = 6L) {

  CheckArgs(abf, channel, allow_list = TRUE)

  if (IsAbf(abf)) {
    ApplyLowpass(abf, chan = channel, freq = freq, order = order)
  } else {
    lapply(abf, ApplyLowpass, chan = channel, freq = freq, order = order)
  }
}
