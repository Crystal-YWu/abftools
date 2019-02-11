IsAbf <- function(x) class(x) == "abf"

IsAbfList <- function(x) {

  IsListOf(x, "abf")
}

IsListOf <- function(x, cls) {

  if (!is.list(x)) {
    FALSE
  } else {
    all(sapply(x, function(item) all(cls %in% class(item))))
  }
}

IsListContains <- function(x, cls) {

  if (!is.list(x)) {
    FALSE
  } else {
    all(sapply(x, function(item) any(cls %in% class(item))))
  }
}

AssertLength <- function(x, ..., explicit = NULL) {

  args <- list(...)
  lenx <- length(x)
  if (any(lenx == explicit)) {
    return(TRUE)
  }
  for (i in seq_along(args)) {
    len <- length(args[[i]])
    if (lenx == len) {
      return(TRUE)
    }
  }

  FALSE
}

AssertEpisode <- function(abf, episode) {

  f <- function(abf, episode) {
    length(episode) && all(nEpi(abf) >= episode & episode > 0L)
  }

  if (IsAbf(abf)) {
    f(abf, episode)
  } else {
    all(sapply(abf, f, episode = episode))
  }
}

AssertChannel <- function(abf, channel) {

  f <- function(abf, channel) {
    length(channel) && all(nChan(abf) >= channel & channel > 0L)
  }

  if (IsAbf(abf)) {
    f(abf, channel)
  } else {
    all(sapply(abf, f, channel = channel))
  }
}

AssertEpoch <- function(abf, epoch) {

  f <- function(abf, epoch) {
    length(epoch) && all(nEpoch(abf) >= epoch & epoch > 0L)
  }

  if (IsAbf(abf)) {
    f(abf, epoch)
  } else {
    all(sapply(abf, f, epoch = epoch))
  }
}

AssertDac <- function(abf, dac) {

  f <- function(abf, dac) {
    nDACNum <- get_meta(abf)$EpochPerDAC$nDACNum
    length(dac) && all((dac - 1L) %in% nDACNum)
  }

  if (IsAbf(abf)) {
    f(abf, dac)
  } else {
    all(sapply(abf, f, dac = dac))
  }
}

CheckArgs <- function(abf, epi = NULL, chan = NULL, epo = NULL, dac = NULL,
                      ..., allow_list = FALSE) {

  isabf <- IsAbf(abf)
  isabflist <- IsAbfList(abf)
  if (!(isabf || (allow_list && isabflist))) {
    err_class_abf()
  }

  if (!is.null(chan) && !AssertChannel(abf, chan)) {
    err_channel()
  }
  if (!is.null(epi) && !AssertEpisode(abf, epi)) {
    err_episode()
  }
  if (!is.null(epo) && !AssertEpoch(abf, epo)) {
    err_epoch()
  }
  if (!is.null(dac) && !AssertDac(abf, dac)) {
    err_epoch_dac()
  }

  TRUE
}
