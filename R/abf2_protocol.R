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
  header <- abf2_header(fp)
  section_info <- abf2_section_info(fp)
  section <- abf2_section(fp, section_info)
  section$Header <- header
  close(fp)

  attr(section, "class") <- "abf_protocol"

  section
}

get_meta <- function(abf) attr(abf, "meta")
