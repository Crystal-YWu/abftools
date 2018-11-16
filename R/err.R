err <- function(msg) {

  func <- as.character(sys.call(-2L))[1]

  err_msg <- paste0(func, ": ", msg)
  stop(err_msg)
}

err_abf_not_episodic <- function() {

  msg <- "The abf object is not episodic."
  err(msg)
}

err_class_abf <- function() {

  msg <- "Only abf object is supported."
  err(msg)
}

err_class_abf_list <- function() {

  msg <- "Only a list of abf objects are supported"
  err(msg)
}

err_class_abf_protocol <- function() {

  msg <- "Only abf or abf protocol object are supported."
  err(msg)
}

err_class_vec_list <- function() {

  msg <- "Only vector or list are supported."
  err(msg)
}

err_intv_pos <- function() {

  msg <- "End position should be larger than start position."
  err(msg)
}

err_id_current_chan <- function() {

  msg <- "Failed to identify current channel id. Please provide manually."
  err(msg)
}

err_id_voltage_chan <- function() {

  msg <- "Failed to identify voltage channel id. Please provide manually."
  err(msg)
}

err_epoch_name <- function() {

  msg <- "Invalid epoch name."
  err(msg)
}

err_epoch <- function() {

  msg <- "Invalid epoch number."
  err(msg)
}

err_wf_mode <- function() {

  msg <- "Episodic stimulation (mode 5) abf object requied."
  err(msg)
}

err_wf_dac <- function() {

  msg <- "Waveform DAC channel not found."
  err(msg)
}

err_wf_support <- function() {

  msg <- "Waveform source not supported."
  err(msg)
}

err_wf_type <- function() {

  msg <- "Unsupported waveform type."
  err(msg)
}

err_epi <- function() {

  msg <- "Invalid episode number."
  err(msg)
}

err_wrong_arg_num <- function(additional = "") {

  msg <- paste0("Wrong number of arguments. ", additional)
  err(msg)
}

err_time_unit <- function() {

  msg <- paste("Invalid time unit, which can only be:",
                "tick, us, ms, s, min or hr.")
  err(msg)
}

err_wrong_dim <- function() {

  msg <- "Dimensions do not match."
  err(msg)
}

err_mask_na <- function() {

  msg <- "You can't mask an episode with NA."
  err(msg)
}

err_internal_bug <- function(additional) {

  msg <- paste0("Internal error, please submit bug report. Addional debug info: ",
                additional)
  err(msg)
}

err_assert_len <- function(var, to_match) {

  msg <- sprintf("Length of %s does not match %s.", var, to_match)

  err(msg)
}

err_epoch_dac <- function() {

  msg <- "Waveform epoch is not recorded in the ABF object."

  err(msg)
}

err_channel <- function() {

  msg <- "Invalid channel id."

  err(msg)
}

err_arrange <- function(arr) {

  msg <- sprintf("Unknown arrangement %s.", as.character(arr))

  err(msg)
}
