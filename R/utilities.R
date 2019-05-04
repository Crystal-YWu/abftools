#' Sample abf object to reduce data points.
#'
#' @param abf an abf object.
#' @param sample_ratio the sampling ratio. See melt.abf for more details.
#' @param sample_func a sampling function applied to sampled points.
#' @param ... arguments passed to sample_func
#'
#' @return a sampled abf object
#' @export
#'
SmplAbf <- function(abf, sample_ratio, sample_func = NULL, ...) {

  CheckArgs(abf)

  data <-  ApplyAbfAttr(x = samplend(abf,
                                     ratio = sample_ratio,
                                     func = sample_func,
                                     along = 1L, ...),
                        abf = abf)

  new_samp_intv <- GetSamplingIntv(abf) * sample_ratio
  attr(data, "SamplingInterval") <- new_samp_intv

  #alter meta
  meta <- get_meta(abf)
  meta$Protocol$fADCSequenceInterval <- new_samp_intv
  if (!is.null(meta$SynchArray)) {
    d <- dim(data)
    if (GetMode(abf) == 1L) {
      nepi <- d[2]
      data[which(is.nan(data))] <- NA
      for (event in seq_len(nepi)) {
        meta$SynchArray$lLength[event] <- sum(!is.na(data[, event, 1L]))
      }
    } else {
      meta$SynchArray$lLength <- d[1] %/% d[3]
    }
  }
  attr(data, "meta") <- meta

  data
}

#' @rdname SmplAbf
#' @export
#'
SampleAbf <- function(abf, sample_ratio, sample_func = NULL, ...) {

  if (IsAbf(abf)) {
    return(
      eval.parent(substitute({
        abf <- SmplAbf(abf, sample_ratio, sample_func, ...)
        abf
      }))
    )
  } else if (IsAbfList(abf)) {
    return(
      eval.parent(substitute({
        abf <- lapply(abf, SmplAbf, sample_ratio = sample_ratio,
                      sample_func = sample_func, ...)
        abf
      }))
    )
  } else {
    err_class_abf()
  }
}

#' Average a list of abf objects.
#'
#' @details Please notice that AverageAbf() does NOT change abf in-place since
#' returned type is changed.
#'
#' @param abf a list of abf objects.
#' @param w OPTIONAL, a vector of weights for weighted average.
#'
#' @return an averaged abf object, of which the protocol settings follow first element in abf_list.
#' @export
#'
AverageAbf <- function(abf, w = NULL) {

  if (!IsAbfList(abf)) {
    err_class_abf_list()
  }

  if (length(abf) == 1L) {
    return(abf[[1]])
  }

  n <- length(abf)
  if (is.null(w)) {
    ret <- Reduce("+", abf) / n
  } else {
    w <- rep_len(w, length.out = n)
    abf <- mapply("*", abf, w, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    ret <- Reduce("+", abf) / sum(w)
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
#' @return an abf object or a list of abf objects
#' @export
#'
BlnkAbf <- function(abf, ref_data, ref_intv = NULL,
                   current_channel = GetFirstCurrentChan(abf)) {

  CheckArgs(abf, chan = current_channel)

  if (IsAbf(ref_data) || IsAbfList(ref_data)) {
    ref_data <- IVSummary(ref_data, intv = ref_intv,
                          current_channel = current_channel,
                          #placeholder voltage_channel, V is not need anyway.
                          voltage_channel = 1L)
  }
  current_data <- ref_data$Current
  abf[,, current_channel] <- sweep(abf[,, current_channel], 2L, current_data)

  abf
}


#' @rdname BlnkAbf
#' @export
#'
BlankAbf <- function(abf, ref_data, ref_intv = NULL, current_channel) {

  if (IsAbf(abf)) {
    eval.parent(substitute({
      abf <- BlnkAbf(abf, ref_data = ref_data, ref_intv = ref_intv,
                     current_channel = current_channel)
      abf
    }))
  } else {
    eval.parent(substitute({
      abf <- lapply(abf, BlnkAbf, ref_data = ref_data, ref_intv = ref_intv,
                    current_channel = current_channel)
      abf
    }))
  }
}
