GetChannelP <- function(abf, channel, ...) {

  df <- melt(abf, channel, ...)
  chan_desc <- GetChannelDesc(abf)[channel]

  p <- ggplot(df, aes_string("time", as.name(chan_desc))) + theme_bw()

  return(p)
}

CollectAllChannel <- function(abf, colour, ...) {

  p <- list()
  n <- nChan(abf)

  for (i in seq_len(n)) {
    p[[i]] <- PlotChannel(abf, i, colour, ...)
  }

  return(p)
}

CollectAllChannel_Intv <- function(abf, intv, colour, ...) {

  p <- list()
  n <- nChan(abf)

  for (i in seq_len(n)) {
    p[[i]] <- PlotChannel_Intv(abf, intv, i, colour, ...)
  }

  return(p)
}

CollectAllChannel_Cursor <- function(abf, cursor, colour, ...) {

  p <- list()
  n <- nChan(abf)

  for (i in seq_len(n)) {
    p[[i]] <- PlotChannel_Cursor(abf, cursor, i, colour, ...)
  }

  return(p)
}

CollectCh <- function(abf, intv = NULL, curs = NULL, channel, colour, time_unit,
                      auto_zoom, ...) {

  #Data
  df <- melt(abf, channel, time_unit = time_unit, ...)
  chan_desc <- GetChannelDesc(abf)[channel]

  #Plot
  p <- ggplot(df, aes_string("time", as.name(chan_desc))) + theme_classic()
  if (colour) {
    p <- p + geom_line(aes_string(colour = "Episode"))
  } else {
    p <- p + geom_line(aes_string(group = "Episode"))
  }

  #intv/cursor
  #check if NA intv is given
  if (any(is.na(intv))) {
    intv <- NULL
  }
  if (!is.null(intv)) {
    intv_tu <- TickToTime(abf, time_unit, intv)
    p <- p + geom_vline(xintercept = intv_tu[1:2], linetype = "dashed")
  }
  if (!is.null(curs)) {
    curs_tu <- TickToTime(abf, time_unit, curs)
    p <- p + geom_vline(xintercept = curs_tu, linetype = "dashed")
  }

  if (auto_zoom) {
    ylimit <- GetYLimit(abf, intv = intv, curs = curs, channel = channel)
    p <- p + ylim(ylimit)
  }

  #Labels
  ydesc <- GetChannelDesc(abf)[channel]
  yunit <- GetChannelUnit(abf)[channel]
  xdesc <- "Time"
  xunit <- time_unit

  xlabel <- GetAxisLabel(xdesc, xunit)
  ylabel <- GetAxisLabel(ydesc, yunit)
  p <- p + xlab(xlabel) + ylab(ylabel)

  #Good to go
  return(p)
}

CollectAllCh <- function(abf, intv = NULL, curs = NULL, colour, time_unit, ...) {

  nch <- nChan(abf)
  plist <- list()

  for (i in seq_len(nch)) {
    plist[[i]] <- CollectCh(abf, intv, curs, i, colour, time_unit, ...)
  }

  return(plist)
}

GetAxisLabel <- function(desc, unit) paste0(desc, " (", unit, ")")
