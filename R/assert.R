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
    ans <- sapply(dac, function(d) {
      all(nEpoch(abf, d) >= epoch & epoch > 0L)
    })
    length(epoch) && all(ans)
  }

  if (IsAbf(abf)) {
    f(abf)
  } else {
    all(sapply(abf, f))
  }
}

AssertDac <- function(abf, dac) {

  f <- function(abf) {
    length(dac) && all(nDAC(abf) >= dac & dac > 0L)
  }

  if (IsAbf(abf)) {
    f(abf)
  } else {
    all(sapply(abf, f))
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

CheckIntvList <- function(abf, intv) {

  if (is.null(intv)) {
    ans <- lapply(abf, function(x) Intv(1L, nPts(x)))
  } else {
    ans <- ExpandList(intv, abf)
    if (is.null(ans)) {
      eval(substitute(err_assert_len(abf, intv, esc_eval = TRUE)))
    }
  }

  ans
}
