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
  #Custom implementation for the missing colSds, may not be most effective
  ret <- colMeans(df * df, na.rm = na.rm) - colMeans(df, na.rm = na.rm)^2
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

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
EpisodesPerChannel <- function(abf) {

  d <- dim(abf)
  if (length(d) == 2)
    return(1L)
  else
    return(d[3])
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
PointsPerEpisode <- function(abf) {

  d <- dim(abf)
  return(d[2])
}

#' Title
#'
#' @param abf
#' @param epi
#' @param value
#'
#' @return
#' @export
#'
#' @examples
MaskEpisodes <- function(abf, epi, value = NA) {

  if (length(dim(abf)) == 2)
    stop("RemoveEpisodes: abf is not episodic.")

  abf[, , epi] <- value

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

  return(MaskEpisodes(abf, epi, NA))
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
AvailEpisodes <- function(abf) {

  if (length(dim(abf)) == 2)
    stop("RemoveEpisodes: abf is not episodic.")

  epi = c()
  for (i in seq_len(EpisodesPerChannel(abf)))
    if (any(!is.na(abf[1, , i])))
      epi = c(epi, i)

  return(epi)
}

