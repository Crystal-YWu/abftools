allowed_delta_abs <- function(channel_data, delta) {

  d <- dim(channel_data)
  ret <- array(data = abs(delta), dim = d)

  return(ret)
}

allowed_delta_rel <- function(channel_data, delta) {

  ret <- abs(channel_data * delta)

  return(ret)
}

#' Title
#'
#' @param abf
#' @param channel
#' @param epoch
#' @param delta
#' @param relative
#'
#' @return
#' @export
#'
#' @examples
CmpWaveform <- function(abf, channel, epoch, delta, relative, min_win, max_win) {

  epoch_win <- GetEpochWindows(abf)
  epoch <- first_elem(epoch)

  episodes <- GetAvailEpisodes(abf)
  wf <- GetWaveform(abf, episodes)
  if (relative) {
    wf_allowed <- allowed_delta_rel(wf, delta)
  } else {
    wf_allowed <- allowed_delta_abs(wf, delta)
  }
  wf_delta <- abs(wf - abf[[channel]])

  ret <- list()
  for (i in seq.int(episodes)) {

    win <- epoch_win[, epoch, episodes[i]]

    mask <- win[1]:win[2]
    v <- wf_delta[mask, i] <= wf_allowed[mask, i]

    tmp <- LogiRleWin(v)

    if (nrow(tmp) > 0L) {
      #shift according to position of win
      tmp[, 1] <- tmp[, 1] + win[1] - 1L
      tmp[, 2] <- tmp[, 2] + win[1] - 1L
      #filter length
      if (!missing(min_win)) {
        tmp_idx <- tmp[, 3] >= min_win
        tmp <- tmp[tmp_idx, ]
      }
      if (!missing(max_win)) {
        tmp_idx <- tmp[, 3] <= max_win
        tmp <- tmp[tmp_idx, ]
      }
    }

    ret[[episodes[i]]] <- tmp
  }

  return(ret)
}


