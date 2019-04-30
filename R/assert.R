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

AssertLength <- function(x, ..., len = NULL) {

  args <- list(...)
  lenx <- length(x)

  if (!is.null(len)) {
    return(lenx == len)
  }
  cmp <- sapply(args, function(item) length(item) == lenx)
  all(cmp)
}

AssertEpisode <- function(abf, episode) {

  f <- function(abf) {
    length(episode) && all(nEpi(abf) >= episode & episode > 0L)
  }

  if (IsAbf(abf)) {
    f(abf)
  } else {
    all(sapply(abf, f))
  }
}

AssertChannel <- function(abf, channel) {

  f <- function(abf) {
    length(channel) && all(nChan(abf) >= channel & channel > 0L)
  }

  if (IsAbf(abf)) {
    f(abf)
  } else {
    all(sapply(abf, f))
  }
}

AssertEpoch <- function(abf, epoch, dac) {

  if (is.null(dac)) {
    dac <- 1L
  }

  f <- function(abf) {
    cmp <- sapply(dac, function(d) {
      all(nEpoch(abf, d) >= epoch)
    })
    length(epoch) && all(epoch > 0) && all(cmp)
  }

  if (IsAbf(abf)) {
    f(abf)
  } else {
    all(sapply(abf, f))
  }
}

AssertDac <- function(abf, dac) {

  f <- function(abf) {
    length(dac) && all(dac > 0L) && all(nDAC(abf) >= dac)
  }

  if (IsAbf(abf)) {
    f(abf)
  } else {
    all(sapply(abf, f))
  }
}

AssertDim <- function(abf, d = NULL) {

  if (!IsAbfList(abf)) {
    TRUE
  } else {
    if (is.null(d)) {
      test <- lapply(abf, dim)
    } else {
      test <- lapply(abf, function(x) dim(x)[d])
    }
    length(unique(test)) == 1L
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
  if (!is.null(epo) && !AssertEpoch(abf, epo, dac)) {
    err_epoch()
  }
  if (!is.null(dac) && !AssertDac(abf, dac)) {
    err_epoch_dac()
  }

  TRUE
}
