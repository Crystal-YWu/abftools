GetChannelP <- function(abf, channel, ...) {

  df <- melt(abf, channel, ...)
  chan_desc <- GetChannelDesc(abf)[channel]

  p <- ggplot(df, aes_string("time", as.name(chan_desc))) + theme_bw()

  return(p)
}

CollectAllChannel <- function(abf, colour, ...) {

  p <- list()
  n <- nChan(abf)

  for (i in seq(n)) {
    p[[i]] <- PlotChannel(abf, i, colour, ...)
  }

  return(p)
}

CollectAllChannel_Intv <- function(abf, intv, colour, ...) {

  p <- list()
  n <- nChan(abf)

  for (i in seq(n)) {
    p[[i]] <- PlotChannel_Intv(abf, intv, i, colour, ...)
  }

  return(p)
}

CollectAllChannel_Cursor <- function(abf, cursor, colour, ...) {

  p <- list()
  n <- nChan(abf)

  for (i in seq(n)) {
    p[[i]] <- PlotChannel_Cursor(abf, cursor, i, colour, ...)
  }

  return(p)
}
