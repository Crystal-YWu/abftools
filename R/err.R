err <- function(msg, esc_eval = FALSE) {

  if (esc_eval) {
    func <- as.character(sys.call(-4L))[1]
  } else {
    func <- as.character(sys.call(-2L))[1]
  }

  err_msg <- paste0(func, ": ", msg)
  stop(err_msg)
}

err_ctype <- function(type) {

  msg <- sprintf("Unknown ctype: %s.", type)
  err(msg)
}

err_abf_file <- function(elaborate) {

  msg <- sprintf("Error reading abf file: %s", elaborate)
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

err_channel <- function() {

  msg <- sprintf("Invalid channel id.")
  err(msg)
}

err_episode <- function() {

  msg <- sprintf("Invalid episode number.")
  err(msg)
}

err_epoch <- function() {

  msg <- "Invalid epoch number."
  err(msg)
}

err_epoch_dac <- function() {

  msg <- "Epoch setting is not enabled for DAC channel."
  err(msg)
}

err_zero_length <- function(x, elaborate = NULL, ...) {

  xname <- as.character(deparse(substitute(x)))
  msg <- sprintf("%s is of zero length.", xname)
  if (!is.null(elaborate)) {
    msg <- sprintf("%s %s", msg, elaborate)
  }

  err(msg, ...)
}

err_abf_not_episodic <- function() {

  msg <- "The abf object is not episodic."
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

  msg <- "Failed to identify current channel id."
  err(msg)
}

err_id_voltage_chan <- function() {

  msg <- "Failed to identify voltage channel id."
  err(msg)
}

err_epoch_name <- function() {

  msg <- "Invalid epoch name."
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

err_wrong_dim <- function(x, y, ...) {

  xname <- as.character(deparse(substitute(x)))
  yname <- as.character(deparse(substitute(y)))
  msg <- sprintf("Dimensions of %s and %s do not match.", xname, yname)
  err(msg, ...)
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

err_assert_len <- function(var, to_match, ...) {

  var_name <- as.character(deparse(substitute(var)))
  match_name <- as.character(deparse(substitute(to_match)))
  msg <- sprintf("Length of %s does not match %s.", var_name, match_name)

  err(msg, ...)
}

err_arrange <- function(arr) {

  msg <- sprintf("Unknown arrangement %s.", as.character(arr))

  err(msg)
}

err_not_func <- function(x) {

  xname <- as.character(deparse(substitute(x)))
  msg <- sprintf("%s is not a function.", xname)

  err(msg)
}

err_chan_id <- function() {

  msg <- "Inconsistent channel identifiers."

  err(msg)
}

err_invalid_axis <- function(axis) {

  if (length(axis > 1L)) {
    axis <- paste(axis, collapse = " ")
  }
  msg <- sprintf("Invalid axis %s.", axis)

  err(msg)
}

err_wrap_func_dim <- function(func) {

  fname <- as.character(deparse(substitute(func)))
  msg <- sprintf("Returned value of %s have length > 1. Please use mapnd() instead.", fname)

  err(msg)
}

err_quick_plot <- function(elaborate = NULL) {

  if (!is.null(elaborate)) {
    msg <- sprintf("Don't know how to plot: %s", elaborate)
  } else {
    msg <- "Don't know how to plot."
  }

  err(msg)
}

err_channel_data <- function(chan) {

  msg <- sprintf("Cannot find channel data: %s.", toString(chan))

  err(msg)
}

err_channel_config <- function(x) {

  xname <- as.character(deparse(substitute(x)))
  msg <- sprintf("Elements of %s have different channel configurations.", xname)

  err(msg)
}
