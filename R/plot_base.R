#' Get an abf time domain ggplot object, dimensionally corresponds to MeltAbf()
abf_plot_td <- function(abf, intv = NULL, channel = 1L,
                        sample_ratio = 1L, sample_func = "mean", ...,
                        time_unit = "tick", colour = FALSE) {

  df <- MeltAbf(abf = abf, intv = intv, channel = channel,
                sample_ratio = sample_ratio, sample_func = sample_func,
                time_unit = time_unit, ..., value.name = "chan")

  xcol <- "Time"
  ycol <- GetChanTag(channel)

  if (colour) {
    p <- ggplot(data = df,
                mapping = aes_string(x = xcol, y = ycol, colour = "Episode"))
  } else {
    p <- ggplot(data = df,
                mapping = aes_string(x = xcol, y = ycol, group = "Episode"))
  }

  p
}

#' Get an abf channel domain ggplot object, dimensionally corresponds to Wrap*()
abf_plot_cd <- function(abf, intv = NULL, channel = c(2L, 1L),
                        map_func = "mean", pack_args = FALSE, ...,
                        colour = FALSE) {

  f <- WrapMappingFuncAlong(map_func = map_func, along = "time", pack_args = pack_args, ...,
                            abf_id_func = GetTitle,
                            epi_id_func = GetEpiTag,
                            chan_id_func = GetChanTag)

  if (IsAbfList(abf)) {
    n <- length(abf)
    intv <- MatchList(intv, n)
    df <- do.call(rbind, lapply(seq_len(n),
                                function(idx) f(abf[[idx]], intv = intv[[idx]], channel = channel)))
  } else {
    df <- f(abf, intv = intv, channel = channel)
  }

  xcol <- GetChanTag(channel[1L])
  ycol <- GetChanTag(channel[2L])

  if (colour) {
    p <- ggplot(data = df,
                mapping = aes_string(x = xcol, y = ycol, colour = "id"))
  } else {
    p <- ggplot(data = df,
                mapping = aes_string(x = xcol, y = ycol, group = "id"))
  }

  p
}

