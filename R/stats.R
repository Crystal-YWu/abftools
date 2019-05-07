do_iv <- function(abf, intv, i_chan, v_chan) {

  force(i_chan)
  force(v_chan)

  if (anyNA(intv)) {
    d <- dim(abf)
    i <- rep(NA, d[2])
    v <- i
  } else {
    if (is.null(intv)) {
      iv <- mapnd_col(abf, matrixStats::colMeans2)
    } else {
      mask <- MaskIntv(intv)
      iv <- mapnd_col(abf[mask,,], matrixStats::colMeans2)
    }
    epi_mask <- !GetEpiAvail(abf)
    if (any(epi_mask)) {
      iv[epi_mask, ] <- NA
    }
    i <- iv[, i_chan]
    v <- iv[, v_chan]
  }

  list(
    i = i,
    v = v
  )
}

do_igv <- function(abf, intv, i_chan, v_chan) {

  force(i_chan)
  force(v_chan)

  if (anyNA(intv)) {
    d <- dim(abf)
    i <- rep(NA, d[2])
    v <- i
    g <- i
  } else {
    if (is.null(intv)) {
      iv <- mapnd_col(abf, matrixStats::colMeans2)
    } else {
      mask <- MaskIntv(intv)
      iv <- mapnd_col(abf[mask,,], matrixStats::colMeans2)
    }
    epi_mask <- !GetEpiAvail(abf)
    if (any(epi_mask)) {
      iv[epi_mask, ] <- NA
    }
    i <- iv[, i_chan]
    v <- iv[, v_chan]
    g <- slope_spline(i, v)
  }

  list(
    i = i,
    v = v,
    g = g
  )
}

#' Report I-V relationship of a list of abf objects.
#'
#' @details This function calculates episodic mean values of current and voltage
#' channels and thier corresponding SEMs given a list of abf objects.
#' IGVSummary() also calulates conductance. However, while averaging conductances
#' gives correct results, SEM of conducetance may not properly represent the actual
#' errors. Though also reported in the returned data.frame, it is not suggested
#' to plot SEM Conductance since it can be mis-interpreted in some cases.
#'
#' @param abf a list of abf objects.
#' @param intv OPTIONAL, a list of intervals.
#' @param conductance Only used in IVSummary(), if TRUE, same as calling IGVSummary()
#' @param current_channel current channel id, 1-based.
#' @param voltage_channel voltage channel id, 1-based.
#' @param group OPTIONAL, add a "Group" column to the returned data.frame.
#' @param unit whether to add unit columns to the returned data.frame.
#' @param raw whether to return raw wide format.
#'
#' @return a data.frame
#' @export
#'
IVSummary <- function(abf, intv = NULL, conductance = FALSE,
                      current_channel = GetFirstCurrentChan(abf),
                      voltage_channel = GetFirstVoltageChan(abf),
                      group = NA, unit = FALSE) {

  if (conductance) {
    IGVSummary(abf, intv = intv,
               current_channel = current_channel, voltage_channel = voltage_channel,
               group = group, unit = unit)
  } else {
    CheckArgs(abf, chan = c(current_channel, voltage_channel), allow_list = TRUE)
    if (!IsAbfList(abf)) {
      abf <- list(abf)
    }
    if (!AssertDim(abf, 2)) {
      stop("Episodes of abf do not match.")
    }
    n <- length(abf)
    intv <- MatchList(intv, n)

    iv <- mapply(do_iv, abf, intv, MoreArgs = list(i_chan = current_channel,
                                                   v_chan = voltage_channel),
                 SIMPLIFY = FALSE,
                 USE.NAMES = FALSE)

    i <- sapply(iv, `[[`, "i")
    v <- sapply(iv, `[[`, "v")
    i_unit <- GetChannelUnit(abf[[1]], channel = current_channel)
    v_unit <- GetChannelUnit(abf[[1]], channel = voltage_channel)

    current <- matrixStats::rowMeans2(i, na.rm = TRUE)
    voltage <- matrixStats::rowMeans2(v, na.rm = TRUE)
    current_sem <- rowSems(i, na.rm = TRUE)
    voltage_sem <- rowSems(v, na.rm = TRUE)

    df <- data.frame(voltage, voltage_sem, current, current_sem, n, group, v_unit, i_unit)
    names(df) <- c("Voltage", "SEM_Voltage", "Current", "SEM_Current", "Num_Samples",
                   "Group", "Unit_Voltage", "Unit_Current")

    cols <- seq_len(5)
    if (!is.na(group)) {
      cols <- c(cols, 6L)
    }
    if (unit) {
      cols <- c(cols, 7L, 8L)
    }

    df[, cols]
  }
}

#' @rdname IVSummary
#' @export
#'
IGVSummary <- function(abf, intv = NULL,
                       current_channel = GetFirstCurrentChan(abf),
                       voltage_channel = GetFirstVoltageChan(abf),
                       group = NA, unit = FALSE, raw = FALSE) {

  CheckArgs(abf, chan = c(current_channel, voltage_channel), allow_list = TRUE)
  if (!IsAbfList(abf)) {
    abf <- list(abf)
  }
  if (!AssertDim(abf, 2)) {
    stop("Episodes of abf do not match.")
  }
  n <- length(abf)
  intv <- MatchList(intv, n)

  igv <- mapply(do_igv, abf, intv,
                MoreArgs = list(i_chan = current_channel,
                                v_chan = voltage_channel),
                SIMPLIFY = FALSE,
                USE.NAMES = FALSE)

  i <- sapply(igv, `[[`, "i")
  g <- sapply(igv, `[[`, "g")
  v <- sapply(igv, `[[`, "v")

  i_unit <- GetChannelUnit(abf[[1]], channel = current_channel)
  v_unit <- GetChannelUnit(abf[[1]], channel = voltage_channel)
  #compose g unit
  i_scale <- parse_unit_scale(i_unit)
  v_scale <- parse_unit_scale(v_unit)
  #dI/dV
  g_scale <- i_scale / v_scale
  g_unit <- paste0(get_unit_prefix(scale = g_scale, long_prefix = FALSE), "S")

  if (raw) {
    #values
    current <- as.data.frame(i)
    voltage <- as.data.frame(v)
    conduct <- as.data.frame(g)
    #id
    id <- names(abf)
    if (is.null(id)) {
      id <- GetTitle(abf)
    }
    epi <- GetEpiLabel(nrow(current))
    #colnames
    names(current) <- id
    names(voltage) <- id
    names(conduct) <- id
    #rownames
    rownames(current) <- epi
    rownames(voltage) <- epi
    rownames(conduct) <- epi
    return(
      list(
        Voltage = voltage,
        Current = current,
        Conduct = conduct
      )
    )
  }

  current <- matrixStats::rowMeans2(i, na.rm = TRUE)
  #averaging conductance still makes sense even if voltage is not consistent,
  #since current ~ voltage and conduct ~ voltage are same measurement.
  conduct <- matrixStats::rowMeans2(g, na.rm = TRUE)
  voltage <- matrixStats::rowMeans2(v, na.rm = TRUE)
  current_sem <- rowSems(i, na.rm = TRUE)
  #however, taking SEMs is not very rigorous, since std of conduct is not same
  #measurement as std current anymore (higher order elements are not constants).
  #It may not mean anything at all.
  conduct_sem <- rowSems(g, na.rm = TRUE)
  voltage_sem <- rowSems(v, na.rm = TRUE)

  df <- data.frame(voltage, voltage_sem,
                   current, current_sem,
                   conduct, conduct_sem, n, group,
                   v_unit, i_unit, g_unit)
  names(df) <- c("Voltage", "SEM_Voltage", "Current", "SEM_Current", "Conductance", "SEM_Conductance",
                 "Num_Samples", "Group", "Unit_Voltage", "Unit_Current", "Unit_Conductance")

  cols <- seq_len(7)
  if (!is.na(group)) {
    cols <- c(cols, 8L)
  }
  if (unit) {
    cols <- c(cols, 9L, 10L, 11L)
  }

  df[, cols]
}

#' Report I-V relationship of a list of abf objects, with specific current/conductance.
#'
#' @param abf a list of abf objects.
#' @param intv OPTIONAL, a list of intervals.
#' @param memprops membrane properties, if NULL memprops will be calculated from StepMemtestSummary().
#' @param conductance Only used in SpIVSummary(), if TRUE, same as calling SpIGVSummary()
#' @param current_channel current channel id, 1-based.
#' @param voltage_channel voltage channel id, 1-based.
#' @param group OPTIONAL, add a "Group" column to the returned data.frame.
#' @param unit whether to add unit columns to the returned data.frame.
#' @param raw whether to return raw wide data.frame.
#' @param ... passed to StepmemtestSummary(), see \code{\link[abftools:StepMemtestSummary]{help}} for details.
#'
#' @return a data.frame
#' @export
#'
SpIVSummary <- function(abf, intv = NULL, conductance = FALSE, memprops = NULL,
                        current_channel = GetFirstCurrentChan(abf),
                        voltage_channel = GetFirstVoltageChan(abf),
                        group = NA, unit = FALSE, ...) {

  if (conductance) {
    SpIGVSummary(abf = abf, intv = intv, memprops = memprops,
                 current_channel = current_channel, voltage_channel = voltage_channel,
                 group = group, unit = unit, ...)
  } else {
    CheckArgs(abf, chan = c(current_channel, voltage_channel), allow_list = TRUE)
    if (!IsAbfList(abf)) {
      abf <- list(abf)
    }
    if (!AssertDim(abf, 2)) {
      stop("Episodes of abf do not match.")
    }
    n <- length(abf)
    intv <- MatchList(intv, n)

    if (is.null(memprops)) {
      memprops <- StepMemtestSummary(abf = abf, current_channel = current_channel, ...)
    }

    iv <- mapply(do_iv, abf, intv, MoreArgs = list(i_chan = current_channel,
                                                   v_chan = voltage_channel),
                 SIMPLIFY = FALSE,
                 USE.NAMES = FALSE)

    i <- sapply(iv, `[[`, "i")
    v <- sapply(iv, `[[`, "v")

    i_unit <- GetChannelUnit(abf[[1]], channel = current_channel)
    v_unit <- GetChannelUnit(abf[[1]], channel = voltage_channel)
    i_scale <- parse_unit_scale(i_unit)

    i_cm <- memprops$Cm / i_scale
    spi <- sweep(i, 2, i_cm, FUN = "/")
    spi_unit <- paste0(i_unit, "/", get_unit_prefix(scale = i_scale, long_prefix = FALSE), "F")

    current <- matrixStats::rowMeans2(i, na.rm = TRUE)
    sp_current <- matrixStats::rowMeans2(spi, na.rm = TRUE)
    voltage <- matrixStats::rowMeans2(v, na.rm = TRUE)

    current_sem <- rowSems(i, na.rm = TRUE)
    sp_current_sem <- rowSems(spi, na.rm = TRUE)
    voltage_sem <- rowSems(v, na.rm = TRUE)

    df <- data.frame(voltage, voltage_sem, current, current_sem, sp_current, sp_current_sem,
                     n, group, v_unit, i_unit, spi_unit)
    names(df) <- c("Voltage", "SEM_Voltage", "Current", "SEM_Current", "SpCurrent", "SEM_SpCurrent",
                   "Num_Samples", "Group", "Unit_Voltage", "Unit_Current", "Unit_SpCurrent")

    cols <- seq_len(7)
    if (!is.na(group)) {
      cols <- c(cols, 8L)
    }
    if (unit) {
      cols <- c(cols, 9L, 10L, 11L)
    }

    df[, cols]
  }
}

#' @rdname SpIVSummary
#' @export
#'
SpIGVSummary <- function(abf, intv = NULL, memprops = NULL,
                         current_channel = GetFirstCurrentChan(abf),
                         voltage_channel = GetFirstVoltageChan(abf),
                         group = NA, unit = FALSE, raw = FALSE, ...) {

  CheckArgs(abf, chan = c(current_channel, voltage_channel), allow_list = TRUE)
  if (!IsAbfList(abf)) {
    abf <- list(abf)
  }
  if (!AssertDim(abf, 2)) {
    stop("Episodes of abf do not match.")
  }
  n <- length(abf)
  intv <- MatchList(intv, n)

  if (is.null(memprops)) {
    memprops <- StepMemtestSummary(abf = abf, current_channel = current_channel, ...)
  }

  igv <- mapply(do_igv, abf, intv,
                MoreArgs = list(i_chan = current_channel,
                                v_chan = voltage_channel),
                SIMPLIFY = FALSE,
                USE.NAMES = FALSE)

  i <- sapply(igv, `[[`, "i")
  g <- sapply(igv, `[[`, "g")
  v <- sapply(igv, `[[`, "v")

  i_unit <- GetChannelUnit(abf[[1]], channel = current_channel)
  v_unit <- GetChannelUnit(abf[[1]], channel = voltage_channel)
  #compose g unit
  i_scale <- parse_unit_scale(i_unit)
  v_scale <- parse_unit_scale(v_unit)
  #dI/dV
  g_scale <- i_scale / v_scale
  g_unit <- paste0(get_unit_prefix(scale = g_scale, long_prefix = FALSE), "S")

  i_cm <- memprops$Cm / i_scale
  g_cm <- memprops$Cm / g_scale
  spi <- sweep(i, 2, i_cm, FUN = "/")
  spg <- sweep(g, 2, g_cm, FUN = "/")
  spi_unit <- paste0(i_unit, "/", get_unit_prefix(i_scale), "F")
  spg_unit <- paste0(g_unit, "/", get_unit_prefix(g_scale), "F")

  if (raw) {
    #values
    current <- as.data.frame(i)
    voltage <- as.data.frame(v)
    conduct <- as.data.frame(g)
    sp_current <- as.data.frame(spi)
    sp_conduct <- as.data.frame(spg)
    #id
    id <- names(abf)
    if (is.null(id)) {
      id <- GetTitle(abf)
    }
    epi <- GetEpiLabel(nrow(current))
    #colnames
    names(current) <- id
    names(voltage) <- id
    names(conduct) <- id
    names(sp_current) <- id
    names(sp_conduct) <- id
    #rownames
    rownames(current) <- epi
    rownames(voltage) <- epi
    rownames(conduct) <- epi
    rownames(sp_current) <- epi
    rownames(sp_conduct) <- epi
    return(
      list(
        Voltage = voltage,
        Current = current,
        Conduct = conduct,
        SpCurrent = sp_current,
        SpConduct = sp_conduct
      )
    )
  }

  current <- matrixStats::rowMeans2(i, na.rm = TRUE)
  conduct <- matrixStats::rowMeans2(g, na.rm = TRUE)
  voltage <- matrixStats::rowMeans2(v, na.rm = TRUE)

  sp_current <- matrixStats::rowMeans2(spi, na.rm = TRUE)
  sp_conduct <- matrixStats::rowMeans2(spg, na.rm = TRUE)

  current_sem <- rowSems(i, na.rm = TRUE)
  sp_current_sem <- rowSems(spi, na.rm = TRUE)
  conduct_sem <- rowSems(g, na.rm = TRUE)
  sp_conduct_sem <- rowSems(spg, na.rm = TRUE)
  voltage_sem <- rowSems(v, na.rm = TRUE)

  df <- data.frame(voltage, voltage_sem, current, current_sem, conduct, conduct_sem,
                   sp_current, sp_current_sem, sp_conduct, sp_conduct_sem,
                   n, group, v_unit, i_unit, g_unit, spi_unit, spg_unit)
  names(df) <- c("Voltage", "SEM_Voltage", "Current", "SEM_Current", "Conductance", "SEM_Conductance",
                 "SpCurrent", "SEM_SpCurrent", "SpConductance", "SEM_SpConductance",
                 "Num_Samples", "Group",
                 "Unit_Voltage", "Unit_Current", "Unit_Conductance", "Unit_SpCurrent", "Unit_SpConductance")

  cols <- seq_len(11)
  if (!is.na(group)) {
    cols <- c(cols, 12L)
  }
  if (unit) {
    cols <- c(cols, 13L, 14L, 15L, 16L, 17L)
  }

  df[, cols]
}

#' Calculate mean values of multiple abf objects and returns in wide format.
#'
#' @param abf_list a list of abf objects.
#' @param intv_list OPTIONAL, a list of intervals.
#' @param channel channel id, 1-based.
#' @param na.rm wheter to remove na values..
#'
#' @return A data.frame object/list of data.frame if multiple channels are used.
#' @export
#'
ChannelMeanWide <- function(abf_list, intv_list = NULL, channel = 1L, na.rm = TRUE) {

  if (!IsAbfList(abf_list)) {
    err_class_abf_list()
  }
  CheckArgs(abf_list, chan = channel, allow_list = TRUE)
  intv_list <- MatchList(intv_list, length(abf_list))

  if (length(channel) == 1L) {
    ret <- as.data.frame(t(Episodic_colFunc(abf_list = abf_list, intv_list = intv_list,
                                            channel = channel, colFunc = matrixStats::colMeans2,
                                            na.rm = na.rm)))
  } else {
    ret <- lapply(channel, function(x) as.data.frame(t(Episodic_colFunc(abf_list = abf_list, intv_list = intv_list,
                                                                        channel = x, colFunc = matrixStats::colMeans,
                                                                        na.rm = na.rm))))
    names(ret) <- GetChannelDesc(abf_list[[1]])[channel]
  }

  ret
}

#' Calculate mean current/voltage of multiple abf objects and return in wide format.
#'
#' @param abf_list a list of abf objects.
#' @param intv_list OPTIONAL, a list of intervals.
#' @param current_channel current channel id.
#' @param voltage_channel voltage channel id.
#' @param na.rm wheter to remove NA values.
#'
#' @return A data.frame object.
#' @export
#'
CurrentMeanWide <- function(abf_list, intv_list = NULL,
                         current_channel = GetFirstCurrentChan(abf_list), na.rm = TRUE) {

  ChannelMeanWide(abf_list = abf_list, intv_list = intv_list,
                  channel = current_channel, na.rm = na.rm)
}

#' @rdname CurrentMeanWide
#' @export
#'
VoltageMeanWide <- function(abf_list, intv_list = NULL,
                         voltage_channel = GetFirstVoltageChan(abf_list), na.rm = TRUE) {

  ChannelMeanWide(abf_list = abf_list, intv_list = intv_list,
                  channel = voltage_channel, na.rm = na.rm)
}

#' Calculate mean values of multiple abf objects and return in long format.
#'
#' @param abf_list a list of abf objects.
#' @param intv_list a list of mean intervals.
#' @param channel channels to calculate.
#' @param group additional grouping info added to columns.
#' @param na.rm whether to remove NAs.
#'
#' @return data.frame
#' @export
#'
ChannelMeanLong <- function(abf_list, intv_list = NULL, channel = NULL, group = NULL, na.rm = TRUE) {

  f <- WrapMappingFunc(mean, channel = channel, abf_id_func = GetTitle, na.rm = na.rm)

  intv_list <- MatchList(intv_list, length(abf_list))
  ans <- mapply(f, abf_list, intv_list, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  ans <- do.call(rbind, ans)
  if (!is.null(group)) {
    ans <- cbind(ans, group)
  }

  ans
}


#' Calculate mean values of an abf object.
#'
#' Returns a matrix/data.frame containing episodic mean values of each channel.
#'
#' @param abf an abf object.
#' @param intv OPTIONAL, an interval to mean over.
#' @param ret.df whether to return a data.frame object, if set to FALSE a matrix is returned instead.
#' @param use_chan_name whether to use channel names as colnames, set to FALSE to use descriptive names instead.
#' @param na.rm whether to remove NA values.
#' @param ... passed to arithmetic mean.
#'
#' @return A matrix/data.frame.
#' @export
#' @method mean abf
#'
mean.abf <- function(abf, intv = NULL, ret.df = FALSE, use_chan_name = FALSE, na.rm = TRUE, ...) {

  if (use_chan_name) {
    chan_id_func <- GetChannelName
  } else {
    chan_id_func <- DefaultChanLabel
  }

  f <- WrapMappingFunc(mean, abf_id_func = NULL, epi_id_func = NULL,
                       chan_id_func = chan_id_func, ret.df = ret.df, na.rm = na.rm, ...)
  ret <- f(abf, intv)
  rownames(ret) <- DefaultEpiLabel(abf)

  ret
}

#' Calculate standard deviation of an abf object.
#'
#' Returns a matrix/data.frame containing episodic sd of each channel.
#'
#' @param abf an abf object.
#' @param intv OPTIONAL, an interval to calculate.
#' @param ret.df whether to return a data.frame object, if set to FALSE a matrix is returned instead.
#' @param use_chan_name whether to use channel names as colnames, set to FALSE to use descriptive names instead.
#' @param na.rm whether to remove NA values.
#'
#' @return A matrix/data.frame.
#' @export
#'
sd_abf <- function(abf, intv = NULL, ret.df = FALSE, use_chan_name = FALSE, na.rm = TRUE) {

  if (use_chan_name) {
    chan_id_func <- GetChannelName
  } else {
    chan_id_func <- DefaultChanLabel
  }

  f <- WrapMappingFunc(stats::sd, abf_id_func = NULL, epi_id_func = NULL,
                       chan_id_func = chan_id_func, ret.df = ret.df, na.rm = na.rm)
  ret <- f(abf, intv)
  rownames(ret) <- DefaultEpiLabel(abf)

  ret
}

sem_func <- function(x, na.rm) stats::sd(x, na.rm) / sqrt(length(x))
#' Calculate standard error of an abf object.
#'
#' Returns a matrix/data.frame containing episodic SEM of each channel.
#'
#' @param abf an abf object.
#' @param intv OPTIONAL, an interval to calculate.
#' @param ret.df whether to return a data.frame object, if set to FALSE a matrix is returned instead.
#' @param use_chan_name whether to use channel names as colnames, set to FALSE to use descriptive names instead.
#' @param na.rm whether to remove NA values.
#'
#' @return A matrix/data.frame.
#' @export
#'
sem_abf <- function(abf, intv = NULL, ret.df = FALSE, use_chan_name = FALSE, na.rm = TRUE) {

  if (use_chan_name) {
    chan_id_func <- GetChannelName
  } else {
    chan_id_func <- DefaultChanLabel
  }

  f <- WrapMappingFunc(sem_func, abf_id_func = NULL, epi_id_func = NULL,
                       chan_id_func = chan_id_func, ret.df = ret.df, na.rm = na.rm)
  ret <- f(abf, intv)
  rownames(ret) <- DefaultEpiLabel(abf)

  ret
}
