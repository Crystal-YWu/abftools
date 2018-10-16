#' Title
#'
#' @param abf
#' @param episodes
#'
#' @return
#' @export
#'
#' @examples
GetWaveform <- function(abf, episodes = 0) {

  if (class(abf) != "abf") {
    err_class_abf("GetWaveform")
  } else if (attr(abf, "mode") != 5L) {
    err_wf_mode("GetWaveform")
  }

  meta <- get_meta(abf)
  #Check DAC channel and DAC source
  wf_dac <- GetWaveformDAC(abf)
  wf_src <- meta$DAC$nWaveformSource[wf_dac]
  if (wf_src != 1L) {
    err_wf_support("GetWaveform")
  }

  #Parse episodes
  nepi <- nEpi(abf)
  if (episodes[1] == 0) {
    episodes = seq.int(nepi)
  }
  if (max(episodes) > nepi) {
    err_epi("GetWaveform")
  }
  #update nepi according to selected episodes
  nepi <- length(episodes)

  #Assume instrument holding. Because I don't know where to extract exact holding
  #values at the moment.
  #throw a warning at this stage
  wf_holding <- meta$DAC$fInstrumentHoldingLevel[wf_dac]
  warning("GetWaveform: Instrument holding is assumed in the generated waveform.")
  npts <- nPts(abf)
  if (nepi == 1L) {
    mx <- rep(wf_holding, npts)
  } else {
    mx <- matrix(wf_holding, nrow = npts, ncol = nepi)
  }

  #Extract epoch settings
  epdac <- GetWaveformEpdac(abf, wf_dac)
  nepoch <- nrow(epdac)

  #Now simulate waveforms
  idx_1stpts <- npts %/% 64L + 1L
  mx_epi_idx <- 0L
  for (epi in episodes) {
    idx <- idx_1stpts
    mx_epi_idx <- mx_epi_idx + 1L
    for (epoch in seq.int(nepoch)) {

      #extract epoch settings
      init_level <- epdac$fEpochInitLevel[epoch]
      incr_level <- epdac$fEpochLevelInc[epoch]
      init_len <- epdac$lEpochInitDuration[epoch]
      incr_len <- epdac$lEpochDurationInc[epoch]
      p_period <- epdac$lEpochPulsePeriod[epoch]
      p_width <- epdac$lEpochPulseWidth[epoch]
      wf_type <- epdac$nEpochType[epoch]

      Vin <- ifelse(nepi > 1L, mx[idx - 1L, mx_epi_idx], mx[idx - 1L])
      Vhi <- init_level + incr_level * (epi - 1L)
      len <- init_len + incr_len * (epi - 1L)

      #calculate simulated waveforms
      tmp <- switch(wf_type,
                    #waveform 1
                    wf_step(len, Vhi),
                    #waveform 2
                    wf_ramp(len, Vin, Vhi),
                    #waveform 3
                    wf_pulse(len, Vin, Vhi, p_period, p_width),
                    #waveform 4
                    wf_trng(len, Vin, Vhi, p_period, p_width),
                    #waveform 5
                    wf_cos(len, Vin, Vhi, p_period),
                    #waveform 6
                    err_wf_type("GetWaveform"),
                    #waveform 7
                    wf_biphsc(len, Vin, Vhi, p_period, p_width),
                    #other
                    err_wf_type("GetWaveform"))

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
    colnames(mx) <- paste0("epi", episodes)
  }
  return(mx)
}

wf_step <- function(len, Vhi) {
  #waveform 1

  return(rep(Vhi, len))
}
wf_ramp <- function(len, Vin, Vhi){
  #waveform 2

  k <- (Vhi - Vin) / len
  return(seq.int(len) * k + Vin)
}
wf_pulse <- function(len, Vin, Vhi, period, width) {
  #waveform 3

  win <- rep(Vin, period)
  win[seq.int(width)] <- Vhi

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
  win <- amp * cos(2 * pi * f * seq.int(period)) - amp + Vin

  ret <- rep(win, length.out = len)
  return(ret)
}
wf_biphsc <- function(len, Vin, Vhi, period, width) {
  #waveform 7

  Vlo <- 2 * Vin - Vhi

  win <- rep(Vin, period)
  hwidth <- (width + 1) %/% 2
  win[1:hwidth] <- Vhi
  win[(hwidth + 1):width] <- Vlo

  ret <- rep(win, length.out = len)

  return(ret)
}
