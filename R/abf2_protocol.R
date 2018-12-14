#functions for parsing various protocol settings

#' Load an abf2 protocol (*.pro) file, or the protocol sections of an abf2 file.
#'
#' Sections will be loaded to seperated data frames with their names. However,
#' strings section is loaded as a list of strings as an exception.
#'
#' @param filename path to the file to load.
#'
#' @return a list of data frames of the loaded protocol sections.
#' @export
#'
abf2_load_protocol <- function(filename) {

  fp <- file(filename, "rb")

  fp <- file(filename, "rb")

  #Read header
  header <- read_struct_n(fp, ABF2.Header.def)
  if (header$fFileSignature != "ABF2") {
    close(fp)
    stop("Only ABF2 file format is supported.")
  }

  #Read all sections info
  section_info <- list()
  for (i in seq_along(ABF2.SectionInfoList)) {
    section_name <- ABF2.SectionInfoList[i]
    section_info[[section_name]] <- read_struct_n(fp, ABF2.SectionInfo.def)
  }

  #Read all supported sections
  section <- list()
  #These sections are quite important, if not presented throw a warning.
  if (section_info$Protocol$llNumEntries > 0) {
    section$Protocol <- read_section(fp, section_info$Protocol, ABF2.Protocol.def, df = FALSE)
  } else {
    warning("Protocol section: no entries recorded.")
  }
  if (section_info$ADC$llNumEntries > 0) {
    section$ADC <- read_section(fp, section_info$ADC, ABF2.ADC.def)
  } else {
    warning("ADC section: no entries recorded.")
  }
  if (section_info$DAC$llNumEntries > 0) {
    section$DAC <- read_section(fp, section_info$DAC, ABF2.DAC.def)
  }
  if (section_info$Epoch$llNumEntries > 0) {
    section$Epoch <- read_section(fp, section_info$Epoch, ABF2.Epoch.def)
  }
  if (section_info$EpochPerDAC$llNumEntries > 0) {
    epdac <- read_section(fp, section_info$EpochPerDAC, ABF2.EpochPerDAC.def)
    section$EpochPerDAC <- epdac[order(epdac$nDACNum, epdac$nEpochNum), ]
  }
  if (section_info$SynchArray$llNumEntries > 0) {
    section$SynchArray <- read_synch_arr_section(fp, section_info$SynchArray)
  }
  if (section_info$Strings$llNumEntries > 0) {
    section$Strings <- read_str_section(fp, section_info$Strings)
  } else {
    warning("Strings section: no entries recorded.")
  }

  attr(section, "class") <- "abf_protocol"

  close(fp)
  return(section)
}

get_meta <- function(abf) {

  if (class(abf) == "abf") {
    meta <- attr(abf, "meta")
  } else if (class(abf) == "abf_protocol") {
    meta <- abf
  } else {
    err_class_abf_protocol()
  }

  return(meta)
}
#' Return the protocols of an abf object.
#'
#' @param abf an abf object.
#'
#' @return a list of data frames of protocol sections.
#' @export
#'
GetProtocol <- function(abf) {

  return(get_meta(abf))
}
