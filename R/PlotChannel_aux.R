CollectCh <- function(abf, channel, intv = NULL, curs = NULL, colour, time_unit,
                      auto_zoom, ...) {

  #Data
  df <- MeltAbf(abf, channel = channel, time_unit = time_unit, ..., value.name = "value")
  ytag <- sprintf("chan%d", channel)

  #Plot
  p <- ggplot(df, aes_string("Time", ytag)) + theme_classic()
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
  #remove NAs from cursor
  curs <- curs[!is.na(curs)]
  if (!is.null(intv)) {
    intv_tu <- TickToTime(intv, time_unit, sampling_rate = abf)
    p <- p + geom_vline(xintercept = intv_tu[1:2], linetype = "dashed")
  }
  if (!is.null(curs)) {
    curs_tu <- TickToTime(curs, time_unit, sampling_rate = abf)
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

  xlabel <- GetAxisLabel(xdesc, xunit, "%s (%s)")
  ylabel <- GetAxisLabel(ydesc, yunit, "%s (%s)")
  p <- p + xlab(xlabel) + ylab(ylabel)

  #Good to go
  p
}

CollectAllCh <- function(abf, ...) {

  nch <- nChan(abf)
  plist <- list()

  for (i in seq_len(nch)) {
    plist[[i]] <- CollectCh(abf, channel = i, ...)
  }

  plist
}

ArrangePlot <- function(p, arrange) {

  pg <- switch(substr(toupper(arrange), 1L, 1L),
               H = cowplot::plot_grid(plotlist = p, nrow = 1, align = "h"),
               V = cowplot::plot_grid(plotlist = p, ncol = 1, align = "v"),
               A = cowplot::plot_grid(plotlist = p),
               err_arrange(arrange))

  pg
}
