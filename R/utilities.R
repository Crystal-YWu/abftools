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

  if (missing(w) || is.null(w)) {
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


  return(ret)
}

ApplyBlank <- function(abf, chan, epi_val) {

  nepi <- nEpi(abf)
  for (epi in seq_len(nepi)) {
    abf[, epi, chan] <- abf[, epi, chan] - epi_val[epi]
  }

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
BlankAbf <- function(abf, ref_data, ref_intv = NULL, current_channel) {

  if (missing(current_channel) || is.null(current_channel)) {
    current_channel <- GetFirstCurrentChan(abf)
  }

  #reference data
  if (IsAbf(ref_data)) {
    ref_data <- mean(ref_data, intv = ref_intv)
  } else if (IsAbfList(ref_data)) {
    ref_data <- IVSummary(ref_data, ref_intv, current_channel = current_channel)
  }
  current_data <- ParseDataFrameIV(ref_data)$Current
  if (any(is.na(current_data))) {
    err_channel_data("Current")
  }

  if (IsAbf(abf)) {
    abf <- ApplyBlank(abf, current_channel, current_data)
  } else if (IsAbfList(abf)) {
    abf <- lapply(abf, ApplyBlank, current_channel, current_data)
  }

  abf
}
