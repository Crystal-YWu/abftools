#' abf2_load
#'
#' @param filename file path to the abf2 file.
#' @param folder path to folder containing the abf2 file.
#' @param abf_title a title assigned to the loaded file, default is filename.
#'
#' @return an abf object.
#' @export
#'
abf2_load <- function(filename, folder, abf_title) {

  if (!missing(folder) && !is.null(folder)) {
    folder <- ifelse(endsWith(folder, "/"), folder, paste0(folder, "/"))
    filename <- paste0(folder, filename)
  }
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
  #These sections are quite important, if not presented throw a warning.
  if (section_info$Protocol$llNumEntries > 0) {
    section$Protocol <- read_section(fp, section_info$Protocol, ABF2.Protocol.def)
  } else {
    close(fp)
    stop("Protocol section: no entries recorded.")
  }
  if (section_info$ADC$llNumEntries > 0) {
    section$ADC <- read_section(fp, section_info$ADC, ABF2.ADC.def)
  } else {
    close(fp)
    stop("ADC section: no entries recorded.")
  }
  if (section_info$DAC$llNumEntries > 0)
    section$DAC <- read_section(fp, section_info$DAC, ABF2.DAC.def)
  if (section_info$Epoch$llNumEntries > 0)
    section$Epoch <- read_section(fp, section_info$Epoch, ABF2.Epoch.def)
  if (section_info$EpochPerDAC$llNumEntries > 0) {
    epdac <- read_section(fp, section_info$EpochPerDAC, ABF2.EpochPerDAC.def)
    #sort EpochPerDAC prior to saving to meta, so we don't need to re-sort this
    #everytime calling epoch.R functions.
    section$EpochPerDAC <- epdac[order(epdac$nDACNum, epdac$nEpochNum), ]
  }
  if (section_info$SynchArray$llNumEntries > 0)
    section$SynchArray <- read_synch_arr_section(fp, section_info$SynchArray)

  #We do not need these miscellaneous sections
  #if (section_info$Math$llNumEntries > 0)
  #  section$Math <- read_section(fp, section_info$Math, ABF2.Math.def)
  #if (section_info$StatsRegion$llNumEntries > 0)
  #  section$StatsRegion <- read_section(fp, section_info$StatsRegion, ABF2.StatsRegion.def)
  #if (section_info$UserList$llNumEntries > 0)
  #  section$UserList <- read_section(fp, section_info$UserList, ABF2.UserList.def)

  #Read strings section
  if (section_info$Strings$llNumEntries > 0)
    section$Strings <- read_str_section(fp, section_info$Strings)
  else
    warning("Strings section: no entries recorded.")
  #Throw warning if read strings do not match llNumEntries.
  if (length(section$Strings) != section_info$String$llNumEntries)
    warning("Strings section: llNumEntries and actual entries read do not match.")

  chan_num <- nrow(section$ADC)
  if (section_info$Strings$llNumEntries == 0) {
    chan_name <- rep("", chan_num)
    chan_unit <- rep("", chan_num)
    chan_desc <- rep("", chan_num)
  } else {
    chan_name <- c()
    chan_unit <- c()
    chan_desc <- c()
    for (i in seq_len(chan_num)) {
      idx <- section$ADC$lADCChannelNameIndex[i]
      chan_name[i] <- section$Strings[[idx]]
      idx <- section$ADC$lADCUnitsIndex[i]
      chan_unit[i] <- section$Strings[[idx]]
      if (endsWith(chan_unit[i], "V") ||
          grepl("vol", chan_unit[i], fixed = TRUE) ||
          grepl("Vol", chan_unit[i], fixed = TRUE)) {
        chan_desc[i] <- "Voltage"
      } else if (endsWith(chan_unit[i], "A") ||
                 grepl("amp", chan_unit[i], fixed = TRUE) ||
                 grepl("Amp", chan_unit[i], fixed = TRUE)) {
        chan_desc[i] <- "Current"
      } else {
        chan_desc[i] <- chan_name[i]
      }
    }
  }

  #As reported here: (Not verified)
  #https://forums.ni.com/t5/LabVIEW/Axon-ABF-amp-ABF2-Files/td-p/3708907
  if (chan_num > 4)
    warning("More than 4 channels are recorded.")

  #Read data section
  #Basic sanity check
  rawdata_int <- section_info$Data$uBytes == 2
  if (section_info$Data$uBytes == 2 || section_info$Data$uByte == 4)
    rawdata <- read_data_section(fp, section_info$Data)
  else {
    close(fp)
    stop("Data section: unknown data record type.")
  }
  #Scaling and offset of rawdata
  if (rawdata_int) {
    signal_resol <- section$Protocol$fADCRange[1] / section$Protocol$lADCResolution[1]
    signal_scale <- rep(1, chan_num)
    signal_offset <- rep(0, chan_num)
    for (i in seq_len(chan_num)) {
      #instrument scale factor
      signal_scale[i] <- section$ADC$fInstrumentScaleFactor[i]
      #signal gain
      signal_scale[i] <- signal_scale[i] * section$ADC$fSignalGain[i]
      #programmable gain
      signal_scale[i] <- signal_scale[i] * section$ADC$fADCProgrammableGain[i]
      #telegraph addit gain
      if (section$ADC$nTelegraphEnable[i])
        signal_scale[i] <- signal_scale[i] * section$ADC$fTelegraphAdditGain[i]
      signal_scale[i] <-  1 / signal_scale[i]
      #offsets: instrument & signal
      signal_offset[i] <- section$ADC$fInstrumentOffset[i] - section$ADC$fSignalOffset[i]
    }
  }

  #sampling interval
  sample_interval_us <- section$Protocol$fADCSequenceInterval[1]

  #parse synch array since sample_interval_us is resolved
  tu_per_tick <- ifelse(section$Protocol$fSynchTimeUnit == 1,
                        yes = sample_interval_us,
                        no = section$Protocol$fSynchTimeUnit)
  lStart_tick <- section$SynchArray$lStart / tu_per_tick
  section$SynchArray <- cbind(section$SynchArray, lStart_tick)

  #We've done all file reading
  close(fp)

  #Now compile everything we've got into result
  op_mode <- section$Protocol$nOperationMode[1]

  if (op_mode == 1L) {
    #event-driven variable-length
    stop("Event-driven variable-length mode is not supported yet.")

    #For variable-length sweeps, we can't simply extract data into a 3d array
    #since sweep lengths (pts_per_chan) are different

    #TODO: a proper data representation of variable-length mode
  }
  else if (op_mode == 2L | op_mode == 4L | op_mode == 5L) {
    #event-driven fixed-length (2), high-speed oscilloscope (4), waveform fixed-length (5)

    #resolve 3d array from rawdata
    pts_per_chan <- section$Protocol$lNumSamplesPerEpisode[1] / chan_num
    chan_per_epi <- chan_num
    epi_per_run <- section$Protocol$lEpisodesPerRun[1]
    #check if data pts number match
    if (section_info$Data$llNumEntries != pts_per_chan * chan_per_epi * epi_per_run) {
      stop("Data section: recorded data points do not match protocol setting.")
    }
    data <- array(data = rawdata, dim = c(chan_per_epi, pts_per_chan, epi_per_run))
    #scale data if needed
    if (rawdata_int)
      for (i in seq_len(chan_per_epi))
        data[i,,] <- data[i,,] * signal_resol * signal_scale[i] + signal_offset[i]

    #data in memory is ordered in (chan, pts, epi) since we usually access
    #episodic data by channel, a more efficient order should be (pts, epi, chan)
    data <- aperm(data, c(2, 3, 1))
  }
  else if (op_mode == 3L) {
    #Gap-free

    #resolve 2d array from rawdata
    #Can't resolve values from protocol any more
    pts_per_chan <- section_info$Data$llNumEntries %/% 2L
    chan_per_run <- chan_num
    #Added 3rd dim so that we can treat a Gap-free like an episodic abf (with only
    #one episode).
    data <- array(data = rawdata, dim = c(chan_per_run, pts_per_chan, 1))
    #scale data if needed
    if (rawdata_int)
      for (i in seq_len(chan_per_run))
        data[i,,] <- data[i,,] * signal_resol * signal_scale[i] + signal_offset[i]

    data <- aperm(data, c(2, 3, 1))
  }
  else {
    stop(paste0("Protocol section: Unrecognised operation mode ", op_mode, "."))
  }

  attr(data, "class") <- "abf"
  if (missing(abf_title) || is.null(abf_title))
    attr(data, "title") <- filename
  else
    attr(data, "title") <- as.character(abf_title)
  attr(data, "mode") <- op_mode

  #attr(data, "ChannelNum") <- chan_num
  attr(data, "ChannelName") <- chan_name
  attr(data, "ChannelUnit") <- chan_unit
  attr(data, "ChannelDesc") <- chan_desc
  attr(data, "SamplingInterval") <- sample_interval_us

  nepi <- dim(data)[2]
  attr(data, "EpiAvail") <- rep(TRUE, nepi)

  meta <- section
  meta$Header <- header
  attr(data, "meta") <- meta

  return(data)
}

#' abf2_loadlist
#'
#' @param filelist a list of file name.
#' @param folder the path to the folder of the files, if filelist contains full path, leave this to empty.
#' @param attach_ext automatically add ".abf" extension to filelist if not present.
#' @param titlelist a list of titles, a single title to be assigned to all loaded files.
#'
#' @return a list of abf objects.
#' @export
#'
abf2_loadlist <- function(filelist, folder, attach_ext = TRUE, titlelist) {

  filelist <- unlist(filelist)
  if (!missing(folder) && !is.null(folder)) {
    folder <- ifelse(endsWith(folder, "/"), folder, paste0(folder, "/"))
    filelist <- paste0(folder, filelist)
  }
  if (attach_ext)
    filelist <- lapply(filelist, function(x) ifelse(endsWith(x, ".abf"), x, paste0(x, ".abf")))

  abf_list <- lapply(filelist, abf2_load)
  #set titles
  if (!missing(titlelist) && !is.null(titlelist)) {
    if (length(titlelist) == 1L) {
      for (i in seq_along(abf_list)) {
        attr(abf_list[[i]], "title") <- as.character(titlelist)
      }
    } else {
      if (length(titlelist) != length(abf_list)) {
        warning("abf2_loadlist: length of titlelist mismatches filelist, ignored.")
      } else {
        for (i in seq_along(abf_list)) {
          attr(abf_list[[i]], "title") <- as.character(titlelist[[i]])
        }
      }
    }
  }

  return(abf_list)
}
