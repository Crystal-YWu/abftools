#' Simulate waveform.
#'
#' @param abf an abf object.
#' @param episodes the episodes to simulate.
#' @param dac waveform DAC channel, 1-based.
#'
#' @return channel data of the simulated waveform.
#' @export
#'
GetWaveform <- function(abf,
                        episodes = GetAllEpisodes(abf),
                        dac = GetWaveformEnabledDAC(abf)) {

  dac <- FirstElement(dac)
  CheckArgs(abf, epi = episodes, dac = dac)
  if (GetMode(abf) != 5L) {
    err_wf_mode()
  }

  meta <- get_meta(abf)
  #Stimulus file is not supported yet
  if (meta$DAC$nWaveformSource[dac] != 1L) {
    err_wf_support()
  }

  nepi <- length(episodes)

  wf_holding <- meta$DAC$fInstrumentHoldingLevel[dac]
  npts <- nPts(abf)
  if (nepi == 1L) {
    mx <- rep(wf_holding, npts)
  } else {
    mx <- matrix(wf_holding, nrow = npts, ncol = nepi)
  }

  #Extract epoch settings
  epdac <- GetEpdac(abf, dac)
  nepoch <- nrow(epdac)

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
      mask <- seq.int(from = idx, length.out = len)
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
    colnames(mx) <- DefaultEpiLabel(episodes)
  }

  mx
}

#' Attach a waveform channel to an abf object.
#'
#' @param abf an abf object.
#' @param dac waveform DAC channel, 1-based.
#'
#' @return an abf object with a new waveform channel attached.
#' @export
#'
AtchWaveform <- function(abf, dac = GetWaveformEnabledDAC(abf)) {

  dac <- FirstElement(dac)
  CheckArgs(abf, dac = dac)

  #get waveform channel
  wf <- GetWaveform(abf, dac = dac)
  #figure out waveform unit
  meta <- get_meta(abf)
  idx_name <- meta$DAC$lDACChannelNameIndex[dac]
  idx_unit <- meta$DAC$lDACChannelUnitsIndex[dac]
  dac_name <- meta$Strings[idx_name]
  dac_unit <- meta$Strings[idx_unit]
  dac_desc <- "Waveform"

  AtchChan(abf, wf, dac_name, dac_unit, dac_desc)
}

#' Attach a waveform channel to an abf object, by-ref like behaviour.
#'
#' @param abf an abf object.
#' @param dac waveform DAC channel, 1-based.
#'
#' @return an abf object with a new waveform channel attached, invisibly.
#' @export
#'
AttachWaveform <- function(abf, dac = GetWaveformEnabledDAC(abf)) {

  eval.parent(substitute({
    abf <- AtchWaveform(abf, dac)
    invisible(abf)
  }))
}

wf_step <- function(len, Vhi) {
  #waveform 1

  rep(Vhi, len)
}
wf_ramp <- function(len, Vin, Vhi){
  #waveform 2

  k <- (Vhi - Vin) / len
  seq_len(len) * k + Vin
}
wf_pulse <- function(len, Vin, Vhi, period, width) {
  #waveform 3

  win <- rep(Vin, period)
  win[seq_len(width)] <- Vhi

  rep(win, length.out = len)
}
wf_trng <- function(len, Vin, Vhi, period, width) {
  #waveform 4

  win_l <- wf_ramp(width, Vhi, Vin)
  if (period == width) {
    rep(win_l, length.out = len)
  } else {
    win_r <- wf_ramp(period - width, Vin, Vhi)
    win <- c(win_l, win_r)
    rep(win, length.out = len)
  }
}
wf_cos <- function(len, Vin, Vhi, period) {
  #waveform 5

  amp <- (Vin - Vhi) / 2
  f <- 1 / period
  win <- amp * cos(2 * pi * f * seq_len(period)) - amp + Vin

  rep(win, length.out = len)
}
wf_biphsc <- function(len, Vin, Vhi, period, width) {
  #waveform 7

  Vlo <- 2 * Vin - Vhi

  win <- rep(Vin, period)
  hwidth <- (width + 1L) %/% 2L
  win[1:hwidth] <- Vhi
  win[(hwidth + 1L):width] <- Vlo

  rep(win, length.out = len)
}
