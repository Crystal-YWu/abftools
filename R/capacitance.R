#' Fit a current curve to a logistic aysmptotic model.
#'
#' Model:
#' i = I0 + (Is - I0) * e^( -exp(lrc) * t )
#'
#' @param i current
#' @param t time
#'
#' @return a list of fitted coefs, and fitted model itself.
#'
fit_charge_i <- function(i, t) {

  f <- stats::formula(I ~ SSasymp(t, Is, I0, lrc))
  df <- data.frame(I = i, t = t)

  fit <- stats::nls(formula = f, data = df)
  coefs <- stats::coef(fit)
  coefs["tao"] <- 1/exp(coefs["lrc"])

  c(as.list(coefs), fitted = list(fit))
}

#' Fit a charging curve and calculate Rs, Rm, Cm
#'
#' @param i current episode
#' @param intv_charge charging interval
#' @param intv_hold hold interval
#' @param delta_v delta voltage
#' @param time_unit time unit to use.
#' @param sampling_rate sampling ratio passed to TickToTime()
#'
#' @return a list of fitted results.
#'
fit_charge_cap <- function(i, intv_charge, intv_hold, delta_v,
                           time_unit = "tick", sampling_rate) {

  charge_start <- intv_hold[2] + 1L
  idx <- MaskIntv(intv_charge)
  if (time_unit == "tick") {
    t <- idx - charge_start
  } else {
    t <- TickToTime(tick = idx - charge_start, time_unit = time_unit, sampling_rate = sampling_rate)
  }
  coefs <- fit_charge_i(i = i[idx], t = t)

  i_hold <- mean(i[MaskIntv(intv_hold)])
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
#' Please also notice that all unit prefixes are converted to "1", thus the
#' returned properties calculated are:
#'
#' Cm: membrane capacitance (in Farad)
#'
#' Rs: series resistance (in Ohms)
#'
#' Rm: membrane resistance (in Ohms)
#'
#' I0: Max charging current (in A)
#'
#' Is: Steady current (in A)
#'
#' tao: RC time constant (in s)
#'
#' @param abf an abf object.
#' @param intv interval of the CHARGING period.
#' @param episode episode of the memtest.
#' @param epoch epoch of the memtest.
#' @param dac DAC channel of Vcmd
#' @param current_channel current channel.
#' @param max_iter maximum iterations.
#'
#' @return a data.frame
#' @export
#'
StepMemtestAbf <- function(abf, intv = NULL, episode = NULL,
                           epoch = FindMemtestEpoch(abf, dac = dac, type = "step"),
                           dac = GetWaveformEnabledDAC(abf),
                           current_channel = GetFirstCurrentChan(abf),
                           max_iter = 10L) {

  dac <- FirstElement(dac)
  current_channel <- FirstElement(current_channel)
  epoch <- FirstElement(epoch)

  CheckArgs(abf, chan = current_channel, epi = episode, epo = epoch, dac = dac)

  abf <- RsclChan(abf, channel = current_channel, scale = "1")

  if (is.null(episode)) {
    episode <- FindStepEpisode(abf, epoch = epoch, dac = dac)
    if (anyNA(episode)) {
      stop("Failed to find an episode suitable for charging current fitting.")
    }
  }

  level_hold <- step_epi_level(abf, epoch - 1L, dac)
  level_charge <- step_epi_level(abf, epoch, dac)
  delta_v <- level_charge - level_hold
  epo <- GetEpochIntervals(abf, dac)

  do_fit_episode <- function(episode) {

    delta_v <- delta_v[episode]
    intv_epoch <- epo[, episode, epoch]
    y <- abf[, episode, current_channel]

    #TODO: hold_intv depending of length of intv is very robust.
    if (!is.null(intv)) {

      #manual intv
      intv_hold <- Intv(endPos = intv_epoch[1] - 1L, len = intv[3])
      #fit
      cap <- fit_charge_cap(i = y,
                            intv_charge = intv, intv_hold = intv_hold, delta_v = delta_v,
                            time_unit = "s", sampling_rate = abf)

    } else {
      min_diff_window <- 5
      diff_window <- min_diff_window
      max_iter <- max_iter + min_diff_window
      #Calculate dy so we don't need to recalculate it every iteration
      dy <- slope_stencil(y, idx = MaskIntv(intv_epoch))

      while (TRUE) {

        #auto intv
        intv <- tryCatch({
          FindChargeInterval(abf,
                             epoch = epoch,
                             dac = dac,
                             episode = episode,
                             current_channel = current_channel,
                             diff_window = diff_window, dy = dy)
        }, error = function(e) {
          NA
        })
        if (anyNA(intv)) {
          cap <- rep(NA, 6)
          warning(sprintf("%s: Failed to find a proper charging interval for episode %d. NAs return.",
                          GetTitle(abf), episode))
          break
        }
        intv_hold <- Intv(endPos = intv_epoch[1] - 1L, len = intv[3])

        #fit
        cap <- tryCatch({
          fit_charge_cap(i = y,
                         intv_charge = intv, intv_hold = intv_hold, delta_v = delta_v,
                         time_unit = "s", sampling_rate = GetSamplingRate(abf))
        }, error = function(e) {
          list(
            Rtot = -1
          )
        })
        if (cap$Rtot < 0 || cap$Rs < 0 || cap$Rm < 0) {
          #Is is not stable, increase diff_window
          diff_window <- diff_window + 1L
          if (diff_window >= max_iter) {
            cap <- rep(NA, 6)
            warning(sprintf("%s: Failed to fit charging curve after %d iterations for episode %d. NAs return.",
                            GetTitle(abf), max_iter - min_diff_window, episode))
            break
          }
        } else {
          cap <- c(cap$Cm, cap$Rs, cap$Rm, cap$coefs$I0, cap$coefs$Is, cap$coefs$tao)
          break
        }
      }
    }

    cap
  }

  ans <- as.data.frame(do.call(rbind, lapply(episode, do_fit_episode)),
                       row.names = DefaultEpiLabel(abf)[episode])
  names(ans) <- c("Cm", "Rs", "Rm", "I0", "Is", "tao")
  ans
}
