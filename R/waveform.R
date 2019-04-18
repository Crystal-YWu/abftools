#' Simulate waveform.
#'
#' @param abf an abf object.
#' @param episode the episodes to simulate.
#' @param dac waveform DAC channel, 1-based.
#'
#' @return channel data of the simulated waveform.
#' @export
#'
GetWaveform <- function(abf,
                        episode = GetAllEpisodes(abf),
                        dac = GetWaveformEnabledDAC(abf)) {

  dac <- FirstElement(dac)
  CheckArgs(abf, epi = episode, dac = dac)
  if (GetMode(abf) != 5L) {
    err_wf_mode()
  }

  meta <- get_meta(abf)
  #Stimulus file is not supported yet
  if (meta$DAC$nWaveformSource[dac] != 1L) {
    err_wf_support()
  }

  nepi <- length(episode)
  npts <- nPts(abf)
  wf_holding <- meta$DAC$fDACHoldingLevel[dac]
  inter_epi <- as.logical(meta$DAC$nInterEpisodeLevel[dac])
  mx <- matrix(wf_holding, nrow = npts, ncol = nepi)

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
  if (npts <= 64) {
    npts_holding <- 1L
  } else {
    npts_holding <- npts %/% 64L
  }
  idx_start <- npts_holding + 1L
  for (i in seq_len(nepi)) {
    idx <- idx_start
    #hold waveform value at last V
    if (inter_epi && i > 1L) {
      mx[seq.int(from = 1L, to = idx - 1L), i] <- mx[npts, i - 1L]
    }
    for (epoch in seq_len(nepoch)) {
      Vin <- mx[idx - 1L, i]
      Vhi <- init_level[epoch] + incr_level[epoch] * (episode[i] - 1L)
      len <- init_len[epoch] + incr_len[epoch] * (episode[i] - 1L)
      wf_epoch <- switch(wf_type[epoch],
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
      mx[seq.int(from = idx, length.out = len), i] <- wf_epoch
      idx <- idx + len
    }
    #hold waveform value at Vin if idx to npts is not defined
    if (inter_epi && idx < npts) {
      Vin <- mx[idx - 1, i]
      mx[seq.int(from = idx, to = npts), i] <- Vin
    }
  }

  #set colnames
  epilabel <- DefaultEpiLabel(abf)
  colnames(mx) <- epilabel[episode]

  mx
}

#' Attach a waveform channel to an abf object.
#'
#'
#' @param abf an abf object.
#' @param dac waveform DAC channel, 1-based.
#'
#' @return an abf object with a new waveform channel attached.
#' @export
#'
AtchWaveform <- function(abf, dac = GetWaveformEnabledDAC(abf)) {

  #dac <- FirstElement(dac)
  CheckArgs(abf, dac = dac)

  tmp <- abf
  meta <- get_meta(abf)
  for (d in dac) {
    #get waveform channel
    wf <- GetWaveform(abf, dac = d)
    #figure out waveform unit
    idx_name <- meta$DAC$lDACChannelNameIndex[d]
    idx_unit <- meta$DAC$lDACChannelUnitsIndex[d]
    dac_name <- meta$Strings[idx_name]
    dac_unit <- meta$Strings[idx_unit]
    dac_desc <- sprintf("Waveform %d", d)
    tmp <- AtchChan(tmp, wf, dac_name, dac_unit, dac_desc)
  }

  tmp
}

#' Attach a waveform channel to an abf object, by-ref like behaviour.
#'
#' @param abf an abf object.
#' @param dac waveform DAC channel, 1-based.
#'
#' @return an abf object with a new waveform channel attached, invisibly.
#' @export
#'
AttachWaveform <- function(abf, dac) {

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

  win_l <- wf_ramp(width, Vin, Vhi)
  if (period == width) {
    rep(win_l, length.out = len)
  } else {
    win_r <- wf_ramp(period - width, Vhi, Vin)
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
