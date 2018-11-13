#' Simulate waveform.
#'
#' @param abf an abf object.
#' @param episodes the episodes to simulate.
#' @param wf_dac_id waveform DAC channel, 1-based.
#'
#' @return channel data of the simulated waveform.
#' @export
#'
GetWaveform <- function(abf, episodes, wf_dac_id) {

  if (!IsAbf(abf)) {
    err_class_abf()
  } else if (attr(abf, "mode") != 5L) {
    err_wf_mode()
  }
  #Check DAC channel and DAC source
  if (missing(wf_dac_id) || is.null(wf_dac_id)) {
    wf_dac_id <- GetWaveformEnabledDAC(abf)
  }
  if (length(wf_dac_id) == 0L) {
    err_wf_dac()
  }
  wf_dac_id <- FirstElement(wf_dac_id)
  #Parse episodes
  nepi <- nEpi(abf)
  if (missing(episodes) || is.null(episodes)) {
    episodes = seq_len(nepi)
  } else if (!AssertEpisode(abf, episodes)) {
    err_epi()
  }
  #update nepi according to selected episodes
  nepi <- length(episodes)

  meta <- get_meta(abf)
  #Stimulus file is not supported yet
  wf_src <- meta$DAC$nWaveformSource[wf_dac_id]
  if (wf_src != 1L) {
    err_wf_support()
  }

  #Assume instrument holding. Because I don't know where to extract exact holding
  #values at the moment.
  #throw a warning at this stage
  wf_holding <- meta$DAC$fInstrumentHoldingLevel[wf_dac_id]
  npts <- nPts(abf)
  if (nepi == 1L) {
    mx <- rep(wf_holding, npts)
  } else {
    mx <- matrix(wf_holding, nrow = npts, ncol = nepi)
  }

  #Extract epoch settings
  epdac <- GetWaveformEpdac(abf, wf_dac_id)
  nepoch <- nrow(epdac)
  if (nepoch == 0L) {
    err_wf_dac()
  }

  #extract epoch settings
  init_level <- epdac$fEpochInitLevel
  incr_level <- epdac$fEpochLevelInc
  init_len <- epdac$lEpochInitDuration
  incr_len <- epdac$lEpochDurationInc
  p_period <- epdac$lEpochPulsePeriod
  p_width <- epdac$lEpochPulseWidth
  wf_type <- epdac$nEpochType

  #Now simulate waveforms
  idx_1stpts <- npts %/% 64L + 1L
  mx_epi_idx <- 0L
  for (epi in episodes) {
    idx <- idx_1stpts
    mx_epi_idx <- mx_epi_idx + 1L
    for (epoch in seq_len(nepoch)) {

      Vin <- ifelse(nepi > 1L, mx[idx - 1L, mx_epi_idx], mx[idx - 1L])
      Vhi <- init_level[epoch] + incr_level[epoch] * (epi - 1L)
      len <- init_len[epoch] + incr_len[epoch] * (epi - 1L)

      #calculate simulated waveforms
      tmp <- switch(wf_type[epoch],
                    #waveform 1
                    wf_step(len, Vhi),
                    #waveform 2
                    wf_ramp(len, Vin, Vhi),
                    #waveform 3
                    wf_pulse(len, Vin, Vhi, p_period[epoch], p_width[epoch]),
                    #waveform 4
                    wf_trng(len, Vin, Vhi, p_period[epoch], p_width[epoch]),
                    #waveform 5
                    wf_cos(len, Vin, Vhi, p_period[epoch]),
                    #waveform 6
                    err_wf_type(),
                    #waveform 7
                    wf_biphsc(len, Vin, Vhi, p_period[epoch], p_width[epoch]),
                    #other
                    err_wf_type())

      #copy tmp to mx
      mask <- seq(from = idx, length.out = len)
      #performance bottleneck is not this if, no need to move out from loop.
      if (nepi > 1L) {
        mx[mask, mx_epi_idx] <- tmp
      } else {
        mx[mask] <- tmp
      }
      idx <- idx + len
    }
  }

  #set colnames
  if (nepi > 1L) {
    colnames(mx) <- paste0("epi", episodes)
  }
  return(mx)
}

#' Attach a waveform channel to an abf object, by-ref behaviour.
#'
#' @param abf an abf object.
#'
#' @return an abf object with a new waveform channel attached.
#' @export
#'
AttachWaveform <- function(abf) {

  if (!IsAbf(abf)) {
    err_class_abf()
  }

  dac <- GetWaveformEnabledDAC(abf)
  if (length(dac) == 0L) {
    err_wf_dac()
  }
  dac <- FirstElement(dac)
  #get waveform channel
  wf <- GetWaveform(abf, wf_dac_id = dac)
  #figure out waveform unit
  meta <- get_meta(abf)
  idx_name <- meta$DAC$lDACChannelNameIndex[dac]
  idx_unit <- meta$DAC$lDACChannelUnitsIndex[dac]
  dac_name <- meta$Strings[idx_name]
  dac_unit <- meta$Strings[idx_unit]
  dac_desc <- "Waveform"

  new_abf <- AtchChan(abf, wf, dac_name, dac_unit, dac_desc)

  return(new_abf)
}

wf_step <- function(len, Vhi) {
  #waveform 1

  return(rep(Vhi, len))
}
wf_ramp <- function(len, Vin, Vhi){
  #waveform 2

  k <- (Vhi - Vin) / len
  return(seq_len(len) * k + Vin)
}
wf_pulse <- function(len, Vin, Vhi, period, width) {
  #waveform 3

  win <- rep(Vin, period)
  win[seq_len(width)] <- Vhi

  ret <- rep(win, length.out = len)
  return(ret)
}
wf_trng <- function(len, Vin, Vhi, period, width) {
  #waveform 4

  win_l <- wf_ramp(width, Vhi, Vin)
  if (period == width) {
    ret <- rep(win_l, length.out = len)
    return(ret)
  }
  win_r <- wf_ramp(period - width, Vin, Vhi)
  win <- c(win_l, win_r)

  ret <- rep(win, length.out = len)
  return(ret)
}
wf_cos <- function(len, Vin, Vhi, period) {
  #waveform 5

  amp <- (Vin - Vhi) / 2
  f <- 1 / period
  win <- amp * cos(2 * pi * f * seq_len(period)) - amp + Vin

  ret <- rep(win, length.out = len)
  return(ret)
}
wf_biphsc <- function(len, Vin, Vhi, period, width) {
  #waveform 7

  Vlo <- 2 * Vin - Vhi

  win <- rep(Vin, period)
  hwidth <- (width + 1L) %/% 2L
  win[1:hwidth] <- Vhi
  win[(hwidth + 1L):width] <- Vlo

  ret <- rep(win, length.out = len)

  return(ret)
}
