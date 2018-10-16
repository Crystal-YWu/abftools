#functions for parsing various protocol settings

#' Title
#'
#' @param fname
#'
#' @return
#' @export
#'
#' @examples
abf2_load_pro <- function(filename) {

  fp <- file(filename, "rb")

  #Read header
  header <- read_struct(fp, ABF2.Header.def)
  if (header$fFileSignature != "ABF2") {
    close(fp)
    stop("Only ABF2 file format is supported.")
  }

  #Read all sections info
  section_info <- list()
  fptr <- header$byte.total
  for (i in seq_along(ABF2.SectionInfoList)) {
    section_name <- ABF2.SectionInfoList[i]
    section_info[[section_name]] <- read_struct(fp, ABF2.SectionInfo.def, fptr)
    fptr <- fptr + section_info[[section_name]]$byte.total
  }

  #Read all supported sections
  section <- list()
  if (section_info$Protocol$llNumEntries > 0)
    section$Protocol <- read_section(fp, section_info$Protocol, ABF2.Protocol.def)
  else {
    close(fp)
    stop("Protocol section: no entries recorded.")
  }
  if (section_info$ADC$llNumEntries > 0)
    section$ADC <- read_section(fp, section_info$ADC, ABF2.ADC.def)
  else {
    close(fp)
    stop("ADC section: no entries recorded.")
  }
  if (section_info$DAC$llNumEntries > 0)
    section$DAC <- read_section(fp, section_info$DAC, ABF2.DAC.def)
  else
    warning("DAC section: no entries recorded.")
  if (section_info$Epoch$llNumEntries > 0)
    section$Epoch <- read_section(fp, section_info$Epoch, ABF2.Epoch.def)
  else
    warning("Epoch section: no entries recorded.")
  if (section_info$EpochPerDAC$llNumEntries > 0)
    section$EpochPerDAC <- read_section(fp, section_info$EpochPerDAC, ABF2.EpochPerDAC.def)
  else
    warning("EpochPerDAC section: no entries recorded.")

  attr(section, "class") <- "abf_protocol"

  close(fp)
  return(section)
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
NumOfChannel <- function(abf) {

  if (class(abf) == "abf") {
    meta <- attr(abf, "meta")
  } else if (class(abf) == "abf_protocol") {
    meta <- abf
  } else {
    err_class_abf_protocol("NumOfChannel")
  }
  #Every observation of table ADC is a channel
  ret <- nrow(meta$ADC)

  return(ret)
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
EpisodesPerChannel <- function(abf) {

  if (class(abf) == "abf") {
    meta <- attr(abf, "meta")
  } else if (class(abf) == "abf_protocol") {
    meta <- abf
  } else {
    err_class_abf_protocol("NumOfChannel")
  }
  ret <- meta$Protocol$lEpisodesPerRun

  return(ret)
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
PointsPerEpisode <- function(abf) {

  if (class(abf) == "abf") {
    meta <- attr(abf, "meta")
  } else if (class(abf) == "abf_protocol") {
    meta <- abf
  } else {
    err_class_abf_protocol("NumOfChannel")
  }
  ret <- meta$Protocol$lNumSamplesPerEpisode

  return(ret)
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
nChan <- function(abf) {

  return(NumOfChannel(abf))
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
nPts <- function(abf) {

  return(PointsPerEpisode(abf))
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
nEpi <- function(abf) {

  return(EpisodesPerChannel(abf))
}

#' Title
#'
#' @param abf
#'
#' @return
#' @export
#'
#' @examples
nPts <- function(abf) {

  return(PointsPerEpisode(abf))
}