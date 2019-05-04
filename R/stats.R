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
#'
#' @return a data.frame
#' @export
#'
IVSummary <- function(abf, intv = NULL, conductance = FALSE,
                      current_channel = GetFirstCurrentChan(abf),
                      voltage_channel = GetFirstVoltageChan(abf),
                      group = NULL) {

  if (conductance) {
    IGVSummary(abf, intv = intv,
               current_channel = current_channel, voltage_channel = voltage_channel,
               group = group)
  } else {
    CheckArgs(abf, chan = c(current_channel, voltage_channel), allow_list = TRUE)
    if (!IsAbfList(abf)) {
      abf <- list(abf)
    }
    if (!AssertDim(abf, 2)) {
      stop("Episodes of abf do not match.")
    }
    n <- length(abf)
    intv <- MatchList(intv, length(abf))

    iv <- mapply(do_iv, abf, intv, MoreArgs = list(i_chan = current_channel,
                                                   v_chan = voltage_channel),
                 SIMPLIFY = FALSE,
                 USE.NAMES = FALSE)

    i <- sapply(iv, `[[`, "i")
    v <- sapply(iv, `[[`, "v")

    current <- matrixStats::rowMeans2(i, na.rm = TRUE)
    voltage <- matrixStats::rowMeans2(v, na.rm = TRUE)
    current_sem <- rowSems(i, na.rm = TRUE)
    voltage_sem <- rowSems(v, na.rm = TRUE)

    if (is.null(group)) {
      df <- data.frame(voltage, voltage_sem, current, current_sem, n)
      colnames(df) <- c("Voltage", "SEM_Voltage",
                        "Current", "SEM_Current",
                        "Num_Samples")
      rownames(df) <- DefaultEpiLabel(nrow(df))
    } else {
      df <- data.frame(voltage, voltage_sem, current, current_sem, n, group)
      colnames(df) <- c("Voltage", "SEM_Voltage",
                        "Current", "SEM_Current",
                        "Num_Samples", "Group")
    }

    df
  }
}

#' @rdname IVSummary
#' @export
#'
IGVSummary <- function(abf, intv = NULL,
                       current_channel = GetFirstCurrentChan(abf),
                       voltage_channel = GetFirstVoltageChan(abf),
                       group = NULL) {

  CheckArgs(abf, chan = c(current_channel, voltage_channel), allow_list = TRUE)
  if (!IsAbfList(abf)) {
    abf <- list(abf)
  }
  if (!AssertDim(abf, 2)) {
    stop("Episodes of abf do not match.")
  }
  n <- length(abf)
  intv <- MatchList(intv, length(abf))

  igv <- mapply(do_igv, abf, intv,
                MoreArgs = list(i_chan = current_channel,
                                v_chan = voltage_channel),
                SIMPLIFY = FALSE,
                USE.NAMES = FALSE)

  i <- sapply(igv, `[[`, "i")
  g <- sapply(igv, `[[`, "g")
  v <- sapply(igv, `[[`, "v")

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

  if (is.null(group)) {
    df <- data.frame(voltage, voltage_sem, current, current_sem, conduct, conduct_sem, n)
    colnames(df) <- c("Voltage", "SEM_Voltage",
                      "Current", "SEM_Current",
                      "Conductance", "SEM_Conductance",
                      "Num_Samples")
    rownames(df) <- DefaultEpiLabel(nrow(df))
  } else {
    df <- data.frame(voltage, voltage_sem, current, current_sem, conduct, conduct_sem, n, group)
    colnames(df) <- c("Voltage", "SEM_Voltage",
                      "Current", "SEM_Current",
                      "Conductance", "SEM_Conductance",
                      "Num_Samples", "Group")
  }

  df
}

#' Calculate mean values of multiple abf objects
#'
#' @param abf_list a list of abf objects.
#' @param intv_list OPTIONAL, a list of intervals.
#' @param channel channel id, 1-based.
#' @param ret.df whether to return a data.frame object, if set to FALSE a matrix is returned instead.
#' @param na.rm wheter to remove na values..
#'
#' @return A data.frame object.
#' @export
#'
MultiMean <- function(abf_list, intv_list = NULL, channel = 1L, ret.df = TRUE,
                      na.rm = TRUE) {

  if (!IsAbfList(abf_list)) {
    err_class_abf_list()
  }
  CheckArgs(abf_list, chan = channel, allow_list = TRUE)
  intv_list <- ExpandIntvList(abf_list, intv_list)

  if (length(channel) == 1L) {
    ret <- t(Episodic_colFunc(abf_list, intv_list, channel, colMeans, na.rm = na.rm))
    if (ret.df) {
      ret <- as.data.frame(ret)
    }
  } else {
    ret <- lapply(channel, function(x) t(Episodic_colFunc(abf_list, intv_list,
                                                          channel = x, f = colMeans,
                                                          na.rm = na.rm)))
    if (ret.df) {
      ret <- lapply(ret, as.data.frame)
    }
    names(ret) <- GetChannelDesc(abf_list[[1]])[channel]
  }

  ret
}

#' Calculate mean currents of multiple abf objects
#'
#' @param abf_list a list of abf objects.
#' @param intv_list OPTIONAL, a list of intervals.
#' @param ret.df whether to return a data.frame object, if set to FALSE a matrix is returned instead.
#' @param na.rm wheter to remove na values..
#'
#' @return A data.frame object.
#' @export
#'
MultiMean_Current <- function(abf_list, intv_list = NULL, ret.df = TRUE, na.rm = TRUE) {

  channel <- GetFirstCurrentChan(abf_list)
  MultiMean(abf_list, intv_list, channel, ret.df, na.rm)
}

#' Calculate mean voltages of multiple abf objects
#'
#' @param abf_list a list of abf objects.
#' @param intv_list OPTIONAL, a list of intervals.
#' @param ret.df whether to return a data.frame object, if set to FALSE a matrix is returned instead.
#' @param na.rm wheter to remove na values.
#'
#' @return A data.frame object.
#' @export
#'
MultiMean_Voltage <- function(abf_list, intv_list = NULL, ret.df = TRUE, na.rm =TRUE) {

  channel <- GetFirstVoltageChan(abf_list)
  MultiMean(abf_list, intv_list, channel, ret.df, na.rm)
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
