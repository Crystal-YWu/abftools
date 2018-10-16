#' Title
#'
#' @param df
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
colSds <- function(df, na.rm = FALSE) {

  n <- ifelse(na.rm, colSums(!is.na(df)), nrow(df))
  #implementation for the missing colSds, may not be most effective
  ret <- colMeans(df * df, na.rm) - colMeans(df, na.rm)^2
  ret <- sqrt(ret * n / (n - 1))

  return(ret)
}

#' Title
#'
#' @param df
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
colSems <- function(df, na.rm = FALSE) {

  sds <- colSds(df, na.rm)
  sqn <- sqrt(length(sds))

  return(sds / sqn)
}

AllAbf <- function(x) {

  if (class(x) != "list")
    return(FALSE)
  else
    return(all(sapply(x, function(y) class(y) == "abf")))
}

#' Title
#'
#' @param abf
#' @param channel
#' @param epi
#' @param value
#' @return
#' @export
#'
#' @examples
MskEpi <- function(abf, channel, epi, value) {

  d <- dim(abf)
  if (d[3] == 1)
    err_abf_not_episodic("MskEpi")

  abf[channel, , epi] <- value

  return(abf)
}
#' Title
#'
#' @param abf
#' @param channel
#' @param epi
#' @param value
#'
#' @return
#' @export
#'
#' @examples
MaskEpisodes <- function(abf, channel, epi, value) {

  if (class(abf) == "abf") {
    return(
      eval.parent(substitute({
        abf <- MskEpi(abf, channel, epi, value)
      }))
    )
  } else if (AllAbf(abf)) {
    warning("MaskEpisodes: masking a list of abf objects.")
    return(
      eval.parent(substitute({
        for (i_____ in seq_along(abf)) {
          abf[[i_____]] <- MskEpi(abf[[i_____]], channel, epi, value)
        }
        rm(i_____)
        #invisible so this function still "returns" a value
        invisible(abf)
      }))
    )
  } else {
    err_class_abf_list("MaskEpisodes")
  }

}

#' Title
#'
#' @param abf
#' @param epi
#'
#' @return
#' @export
#'
#' @examples
RmEpi <- function(abf, epi) {

  d <- dim(abf)
  if (d[3] == 1)
    err_abf_not_episodic("RmEpi")

  abf[, , epi] <- NA

  return(abf)
}
#' Title
#'
#' @param abf
#' @param epi
#'
#' @return
#' @export
#'
#' @examples
RemoveEpisodes <- function(abf, epi) {

  if (class(abf) == "abf") {
    return(
      eval.parent(substitute({
        abf <- RmEpi(abf, epi)
      }))
    )
  } else if (AllAbf(abf)) {
    warning("RemoveEpisodes: removing episodes from a list of abf objects.")
    return(
      eval.parent(substitute({
        for (i_____ in seq_along(abf)) {
          abf[[i_____]] <- RmEpi(abf[[i_____]], epi)
        }
        rm(i_____)
        #invisible so this function still "returns" a value
        invisible(abf)
      }))
    )
  } else {
    err_class_abf_list("RemoveEpisodes")
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
GetAvailEpisodes <- function(abf) {

  f <- function(x) {
    epi = c()
    for (i in seq.int(nEpi(x)))
      if (any(!is.na(x[1, , i])))
        epi = c(epi, i)

      return(epi)
  }

  if (class(abf) == "abf") {
    return(f(abf))
  } else if (AllAbf(abf)) {
    return(lapply(abf, f))
  } else {
    err_class_abf_list("GetAvailEpisodes")
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
GetTitle <- function(abf) {

  if (class(abf) == "abf") {
    return(attr(abf, "title"))
  } else if (AllAbf(abf)) {
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
  } else if (AllAbf(abf)) {
    if (length(title) == 1) {
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
#' @param intv
#' @param startPos
#' @param endPos
#'
#' @return
#' @export
#'
#' @examples
SetIntv <- function(intv, startPos, endPos) {

  if (startPos >= endPos) {
    err_intv_pos("SetIntv")
  }
  if (abs(endPos - startPos) < 3L) {
    warning("SetIntv: interval size smaller than 3.")
  }

  eval.parent(substitute({
    intv <- c(startPos, endPos, endPos - startPos + 1L)
  }))
}

copy_paste_place_holder <- function() {
  if (class(abf) == "abf") {
  } else if (AllAbf(abf)) {
  } else {
    err_class_abf_list("")
  }
}