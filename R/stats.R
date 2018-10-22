#' IVSummary calculates average current of a list of abf objects, within given intervals
#'
#' @param abf_list a list of abf objects.
#' @param intv_list a list of intervals.
#' @param current_channel current channel id, 1-based.
#' @param voltage_channel voltage channel id, 1-based.
#'
#' @return a data frame containing calculated Voltage, SEM Voltage, Current, SEM Current columns.
#' @export
#'
IVSummary <- function(abf_list, intv_list, current_channel = 0, voltage_channel = 0) {

  #figure out current channel and voltage channel
  if (current_channel == 0) {
    current_channel <- GetFirstCurrentChan(abf_list[[1]])
  }
  if (voltage_channel == 0) {
    voltage_channel <- GetFirstVoltageChan(abf_list[[1]])
  }
  if (is.na(current_channel)) {
    err_id_current_chan("AllSamples_IVSummary")
  }
  if (is.na(voltage_channel)) {
    err_id_voltage_chan("AllSamples_IVSummary")
  }

  current_means <- EpisodicIntervalMeans(abf_list, intv_list, current_channel)
  voltage_means <- EpisodicIntervalMeans(abf_list, intv_list, voltage_channel)

  mean_current_means <- colMeans(current_means, na.rm = TRUE)
  mean_voltage_means <- colMeans(voltage_means, na.rm = TRUE)
  sem_current_means <- colSems(current_means, na.rm = TRUE)
  sem_voltage_means <- colSems(voltage_means, na.rm = TRUE)

  df <- data.frame(mean_voltage_means, sem_voltage_means, mean_current_means, sem_current_means)
  colnames(df) <- c("Voltage", "SEM Voltage", "Current", "SEM Current")
  return(df)
}
