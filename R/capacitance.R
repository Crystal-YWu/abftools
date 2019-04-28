fit_charge_i <- function(i, t) {

  f <- stats::formula(I ~ SSasymp(t, Is, I0, lrc))
  df <- data.frame(I = i, t = t)

  fit <- stats::nls(formula = f, data = df)
  coefs <- stats::coef(fit)
  coefs["tao"] <- 1/exp(coefs["lrc"])

  c(as.list(coefs), fitted = fit)
}

fit_charge_cap <- function(i, charge_intv, hold_intv, delta_v,
                           time_unit = "tick", sampling_rate) {

  charge_start <- hold_intv[2] + 1L
  idx <- MaskIntv(charge_intv)
  if (time_unit == "tick") {
    t <- idx - charge_start
  } else {
    t <- TickToTime(tick = idx - charge_start, time_unit = time_unit, sampling_rate = sampling_rate)
  }
  coefs <- fit_charge_i(i = i[idx], t = t)

  i_hold <- mean(i[MaskIntv(hold_intv)])
  delta_i <- coefs$Is - i_hold

  #Rm + Rs:
  RmRs <- delta_v / delta_i
  Rs <- delta_v / coefs$I0
  Rm <- RmRs - Rs
  Cm <- coefs$tao * (1/Rm + 1/Rs)

  list(
    Rtot = RmRs,
    Rs = Rs,
    Rm = Rm,
    Cm = Cm,
    coefs = coefs
  )
}

#' Calculate membrane properties from a step pulse membrane test.
#'
#' @details This function calculates membreane properties given a proper step
#' pulse is applied and recorded in the abf object. Usually all arguments can
#' be determined automatically. By default episode is determined by
#' \code{\link[abftools:FindStepEpisode]{FindStepEpisode()}}, see help for more
#' details.
#'
#' The returned list contains membrane properties calculated, which are:
#'
#' Rtot: total resistance (in Ohms)
#'
#' Rs: series resistance (in Ohms)
#'
#' Rm: membrane resistance (in Ohms)
#'
#' Cm: membrane capacitance (in Farad)
#'
#' coefs: Fitted coefficients for debug purpose.
#'
#' coefs$Is: Steady current (in A)
#'
#' coefs$I0: Max current (in A)
#'
#' coefs$lrc: log rate constant (arbitrary unit, logarithm of second)
#'
#' coefs$tao: RC time constant (in s)
#'
#' coefs$fitted: fitted model.
#'
#'
#'
#' @param abf an abf object.
#' @param intv interval of the CHARGING period.
#' @param episode episode of the memtest.
#' @param epoch epoch of the memtest.
#' @param dac DAC channel of Vcmd
#' @param current_channel current channel.
#' @param max_iter maximum iterations.
#' @param simplify whether to return a flat simplified named vector.
#'
#' @return a list of membrane properties, see Details.
#' @export
#'
StepMemtestAbf <- function(abf, intv = NULL, episode = NULL,
                           epoch = FindMemtestEpoch(abf, dac = dac, type = "step"),
                           dac = GetWaveformEnabledDAC(abf),
                           current_channel = GetFirstCurrentChan(abf),
                           max_iter = 30L, simplify = TRUE) {

  dac <- FirstElement(dac)
  current_channel <- FirstElement(current_channel)
  epoch <- FirstElement(epoch)

  CheckArgs(abf, chan = current_channel, epi = episode, epo = epoch, dac = dac)

  abf <- RsclChan(abf, channel = current_channel, scale = "1")

  if (is.null(episode)) {
    episode <- FindStepEpisode(abf, epoch = epoch, dac = dac)
    if (is.na(episode)) {
      stop("Failed to find an episode suitable for charging current fitting.")
    }
  }
  episode <- FirstElement(episode)

  hold_epi <- step_epi_level(abf, epoch - 1L, dac)
  charge_epi <- step_epi_level(abf, epoch, dac)
  delta_v <- charge_epi - hold_epi
  delta_v <- delta_v[episode] / 1000

  epo <- GetEpochIntervals(abf, dac)
  epo_start <- epo[1, episode, epoch]
  if (!is.null(intv)) {
    #manual intv
    hold_intv <- Intv(endPos = epo_start - 1L, len = intv[3])
    fit_charge_cap(i = abf[, episode, current_channel],
                   charge_intv = intv, hold_intv = hold_intv, delta_v = delta_v,
                   time_unit = "s", sampling_rate = GetSamplingRate(abf))
  } else {
    #auto intv
    diff_window <- 5L
    #Calculate dy so we don't need to recalculate it every iteration
    y <- abf[, episode, current_channel]
    stencil <- seq.int(-1, 1)
    coefs <- stencil_coefs(stencil = stencil, order = 1L)
    dy <- sapply(MaskIntv(epo[, episode, epoch]), function(idx) {
      stencil_finite_diff(y, idx, stencil = stencil, order = 1, coefs = coefs)
    })
    while (TRUE) {
      #intv
      intv <- FindChargeInterval(abf,
                                 epoch = epoch,
                                 dac = dac,
                                 episode = episode,
                                 current_channel = current_channel,
                                 diff_window = diff_window, dy = dy)
      hold_intv <- Intv(endPos = epo_start - 1L, len = intv[3])
      #fit
      cap <- tryCatch({
        fit_charge_cap(i = abf[, episode, current_channel],
                       charge_intv = intv, hold_intv = hold_intv, delta_v = delta_v,
                       time_unit = "s", sampling_rate = GetSamplingRate(abf))
      }, error = function(e) {
        msg <- paste("StepMemtestAbf", GetTitle(abf), ":", e)
        warning(msg)
        list(
          Rtot = -1
        )
      })
      if (cap$Rtot < 0) {
        #Is is not stable, increase diff_window
        diff_window <- diff_window + 1L
        if (diff_window - 5L >= max_iter) {
          stop(sprintf("Failed to fit charging curve after %d iterations.", max_iter))
        }
      } else {
        break
      }
    }
  }

  if (simplify) {
    ans <- c(cap$Cm, cap$Rs, cap$Rm, cap$coefs$I0, cap$coefs$Is)
    names(ans) <- c("Cm", "Rs", "Rm", "I0", "Is")
    ans
  } else {
    cap
  }
}
