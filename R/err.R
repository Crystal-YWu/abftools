err <- function(func, msg) {

  err_msg <- paste0(func, ": ", msg)
  stop(err_msg)
}

err_abf_not_episodic <- function(func) {

  msg <- "The abf object is not episodic."
  err(func, msg)
}

err_class_abf <- function(func) {

  msg <- "Only abf object is supported."
  err(func, msg)
}

err_class_abf_list <- function(func) {

  msg <- "Only abf or a list of abf objects are supported"
  err(func, msg)
}

err_class_abf_protocol <- function(func) {

  msg <- "Only abf or abf protocol object are supported."
  err(func, msg)
}

err_intv_pos <- function(func) {

  msg <- "End position should be larger than start position."
  err(func, msg)
}

err_id_current_chan <- function(func) {

  msg <- "Failed to identify current channel id. Please provide manually."
  err(func, msg)
}

err_id_voltage_chan <- function(func) {

  msg <- "Failed to identify voltage channel id. Please provide manually."
  err(func, msg)
}

err_epoch_name <- function(func) {

  msg <- "Invalid epoch name."
  err(func, msg)
}

err_epoch <- function(func) {

  msg <- "Invalid epoch number."
  err(func, msg)
}

err_wf_mode <- function(func) {

  msg <- "Episodic stimulation (mode 5) abf object requied."
  err(func, msg)
}

err_wf_dac <- function(func) {

  msg <- "Waveform DAC channel not found."
  err(func, msg)
}

err_wf_support <- function(func) {

  msg <- "Waveform source not supported."
  err(func, msg)
}

err_wf_type <- function(func) {

  msg <- "Unsupported waveform type."
  err(func, msg)
}

err_epi <- function(func) {

  msg <- "Invalid episode number."
  err(func, msg)
}

err_wrong_arg_num <- function(func, additional = "") {

  msg <- paste0("Wrong number of arguments. ", additional)
  err(func, msg)
}

err_time_unit <- function(func) {

  msg <- paste("Invalid time unit, which can only be:",
                "tick, us, ms, s, min or hr.")
  err(func, msg)
}

err_wrong_dim <- function(func) {

  msg <- "Dimensions do not match."
  err(func, msg)
}

err_mask_na <- function(func) {

  msg <- "You can't mask an episode with NA."
  err(func, msg)
}
