#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
GetTitle <- function(abf) {

  if (class(abf) == "abf") {
    return(attr(abf, "title"))
  } else if (IsAbfList(abf)) {
    return(lapply(abf, function(x) attr(x, "title")))
  } else {
    err_class_abf_list("GetTitle")
  }

}

#' Title
#'
#' @param abf
#' @param title
#'
#' @return
#' @export
#'
#' @examples
SetTitle <- function(abf, title) {

  if (class(abf) == "abf") {
    eval.parent(substitute({
      attr(abf, "title") <- as.character(title)
      invisible(abf)
    }))
  } else if (IsAbfList(abf)) {
    if (length(title) == 1L) {
      eval.parent(substitute({
        for (i_____ in seq_along(abf)) {
          attr(abf[[i_____]], "title") <- as.character(title)
        }
        rm(i_____)
        invisible(abf)
      }))
    } else {
      eval.parent(substitute({
        for (i_____ in seq_along(abf)) {
          attr(abf[[i_____]], "title") <- as.character(title[[i_____]])
        }
        rm(i_____)
        invisible(abf)
      }))
    }
  } else {
    err_class_abf_list("SetTitle")
  }
}


#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
GetChannelName <- function(abf) {

  return(attr(abf, "ChannelName"))
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
GetChannelUnit <- function(abf) {

  return(attr(abf, "ChannelUnit"))
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
GetChannelDesc <- function(abf) {

  return(attr(abf, "ChannelDesc"))
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
GetSamplingIntv <- function(abf) {

  return(attr(abf, "SamplingInterval"))
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
GetMode <- function(abf) {

  return(attr(abf, "mode"))
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
GetNumOfChannel <- function(abf) {

  meta <- get_meta(abf)
  #Every observation of table ADC is a channel
  ret <- nrow(meta$ADC)

  return(ret)
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
GetEpisodesPerChannel <- function(abf) {

  mode <- GetMode(abf)
  if (mode == 3L) {
    return(1L)
  }

  meta <- get_meta(abf)
  ret <- meta$Protocol$lEpisodesPerRun

  return(ret)
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
GetPointsPerEpisode <- function(abf) {

  mode <- GetMode(abf)
  if (mode == 3L) {
    return(dim(abf)[1])
  }

  meta <- get_meta(abf)
  ret <- meta$Protocol$lNumSamplesPerEpisode %/% GetNumOfChannel(abf)

  return(ret)
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
nChan <- function(abf) {

  return(GetNumOfChannel(abf))
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
nPts <- function(abf) {

  return(GetPointsPerEpisode(abf))
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
nEpi <- function(abf) {

  return(GetEpisodesPerChannel(abf))
}
