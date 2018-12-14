#' abf2_load
#'
#' @param filename file path to the abf2 file.
#' @param folder OPTIONAL, path to folder containing the abf2 file.
#' @param abf_title OPTIONAL, a title assigned to the loaded file, default is filename.
#'
#' @return an abf object.
#' @export
#'
abf2_load <- function(filename, folder = NULL, abf_title = NULL) {

  if (is.null(abf_title)) {
    #strip all leading path for abf_title
    tmp <- strsplit(as.character(filename), "/", fixed = TRUE)[[1]]
    abf_title <- tmp[length(tmp)]
  } else {
    abf_title <- as.character(abf_title)
  }
  if (!is.null(folder)) {
    folder <- AddSurfix(folder, "/")
    filename <- paste0(folder, filename)
  }

  fp <- file(filename, "rb")

  header <- abf2_head(fp)
  section_info <- abf2_section_info(fp)
  section <- abf2_section(fp, section_info)

  #sampling interval
  sample_interval_us <- section$Protocol$fADCSequenceInterval
  #tu_per_tick <- ifelse(section$Protocol$fSynchTimeUnit == 1,
  #                      yes = sample_interval_us,
  #                      no = section$Protocol$fSynchTimeUnit)
  #lStart_tick <- section$SynchArray$lStart %/% tu_per_tick + 1L
  #section$SynchArray$lStart <- lStart_tick

  chan_num <- nrow(section$ADC)
  chan_name <- rep("", chan_num)
  chan_unit <- rep("", chan_num)
  chan_desc <- rep("", chan_num)
  if (!is.null(section$Strings)) {
    for (i in seq_len(chan_num)) {
      idx <- section$ADC$lADCChannelNameIndex[i]
      if (idx != 0) {
        chan_name[i] <- section$Strings[idx]
      } else {
        #skip if the channel is unnamed.
        next
      }
      idx <- section$ADC$lADCUnitsIndex[i]
      if (idx != 0) {
        chan_unit[i] <- section$Strings[[idx]]
      }
      if (endsWith(toupper(chan_unit[i]), "V") ||
          grepl("VO", toupper(chan_unit[i]), fixed = TRUE)) {
        chan_desc[i] <- "Voltage"
      } else if (endsWith(toupper(chan_unit[i]), "A") ||
                 grepl("AM", toupper(chan_unit[i]), fixed = TRUE)) {
        chan_desc[i] <- "Current"
      } else {
        chan_desc[i] <- gsub("\\s", "_", chan_name[i])
      }
    }
  }

  #Read data section
  data_int <- section_info$Data$uBytes == 2
  data <- read_data_section(fp, section_info$Data)

  #Scaling and offset of rawdata
  if (data_int) {
    signal_resol <- section$Protocol$fADCRange / section$Protocol$lADCResolution
    signal_scale <- rep(1.0, chan_num)
    signal_offset <- rep(0.0, chan_num)
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
      signal_scale[i] <- 1.0 / signal_scale[i]
      #offsets: instrument & signal
      signal_offset[i] <- section$ADC$fInstrumentOffset[i] - section$ADC$fSignalOffset[i]
    }
  }

  #We've done all file reading
  close(fp)

  #Now compile everything we've got into result
  op_mode <- section$Protocol$nOperationMode
  if (op_mode == 1L) {
    #event-driven variable-length
    warning("Event-driven variable-length mode is not tested.")

    #NA padded 3d array
    max_length <- max(section$SynchArray$lLength)
    nevent <- nrow(section$SynchArray)
    npts <- max_length %/% chan_num
    tmp <- array(data = NA, dim = c(chan_num, npts, nevent))

    idx_end <- cumsum(section$SynchArray$lLength)
    idx_start <- c(1L, idx_end + 1L)

    for (i in seq_len(nevent)) {
      idx <- seq.int(from = idx_start[i], to = idx_end[i])
      tmp[,,i] <- data[idx]
    }
    data <- tmp

  }
  else if (op_mode == 2L | op_mode == 4L | op_mode == 5L) {
    #event-driven fixed-length (2), high-speed oscilloscope (4), waveform fixed-length (5)
    pts_per_chan <- section$Protocol$lNumSamplesPerEpisode %/% chan_num
    epi_per_run <- section$Protocol$lEpisodesPerRun
    #check if data pts number match
    if (section_info$Data$llNumEntries != pts_per_chan * chan_num * epi_per_run) {
      err_abf_file("Recorded number of data points does not match protocol setting.")
    }
    dim(data) <- c(chan_num, pts_per_chan, epi_per_run)

  }
  else if (op_mode == 3L) {
    #Gap-free
    #Can't resolve values from protocol any more
    pts_per_chan <- section_info$Data$llNumEntries %/% chan_num
    dim(data) <- c(chan_num, pts_per_chan, 1L)
  }
  else {
    err_abf_file(sprintf("Unrecognised op mode %d", op_mode))
  }

  data <- aperm(data, c(2, 3, 1))
  if (data_int) {
    dscale <- signal_resol * signal_scale
    doffset <- signal_offset
    for (i in seq_len(chan_num)) {
      data[,,i] <- data[,,i] * dscale[i] + doffset[i]
    }
  }

  attr(data, "class") <- "abf"
  attr(data, "title") <- abf_title
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
#' @param filelist a list of file name or a data.frame like object with a column named FileName/filename.
#' @param folder OPTIONAL, the path to the folder of the files, if filelist contains full path, leave this to empty.
#' @param attach_ext automatically add ".abf" extension to filelist if not present.
#' @param titlelist OPTIONAL, a list of titles, a single title to be assigned to all loaded files.
#'
#' @return a list of abf objects.
#' @export
#'
abf2_loadlist <- function(filelist, folder = NULL, attach_ext = TRUE, titlelist = NULL) {

  names(filelist) <- tolower(names(filelist))

  if (is.object(filelist)) {
    #filename
    if ("filename" %in% names(filelist)) {
      extracted <- filelist$filename
    } else
    #filenames
    if ("filenames" %in% names(filelist)) {
      extracted <- filelist$filenames
    } else
    #file
    if ("file" %in% names(filelist)) {
      extracted <- filelist$file
    } else
    #files
    if ("files" %in% names(filelist)) {
      extracted <- filelist$files
    } else {
      #use first column directly
      extracted <- filelist[[1]]
    }
  } else {
    extracted <- filelist
  }

  filelist <- as.character(unlist(extracted))

  if (!is.null(folder)) {
    folder <- AddSurfix(as.character(folder), "/")
  }
  if (attach_ext) {
    filelist <- lapply(filelist, function(x) AddSurfix(x, ".abf"))
  }

  #load abf
  abf_list <- lapply(filelist, function(x) abf2_load(x, folder, NULL))

  #set titles
  if (!is.null(titlelist)) {
    titlelist <- as.character(unlist(titlelist))
    SetTitle(abf_list, titlelist)
  }

  return(abf_list)
}

AddSurfix <- function(x, sur) ifelse(endsWith(x, sur), x, paste0(x, sur))

abf2_head <- function(fp) {

  header <- read_struct_n(fp, ABF2.Header.def)
  if (header$fFileSignature != "ABF2") {
    close(fp)
    err_abf_file("Only ABF2 file format is supported.")
  }

  header
}

abf2_section_info <- function(fp) {

  section_info <- list()
  for (i in seq_along(ABF2.SectionInfoList)) {
    section_name <- ABF2.SectionInfoList[i]
    section_info[[section_name]] <- read_struct_n(fp, ABF2.SectionInfo.def)
  }

  section_info
}

abf2_section <- function(fp, section_info) {

  #Read all supported sections
  section <- list()
  #These sections are quite important, if not presented throw a warning.
  if (section_info$Protocol$llNumEntries > 0) {
    section$Protocol <- read_section(fp, section_info$Protocol, ABF2.Protocol.def, df = FALSE)
  } else {
    close(fp)
    err_abf_file("No protocol entries recorded.")
  }
  if (section_info$ADC$llNumEntries > 0) {
    section$ADC <- read_section(fp, section_info$ADC, ABF2.ADC.def)
  } else {
    close(fp)
    err_abf_file("No ADC entries recorded.")
  }
  if (section_info$DAC$llNumEntries > 0) {
    section$DAC <- read_section(fp, section_info$DAC, ABF2.DAC.def, df = FALSE)
  }
  if (section_info$Epoch$llNumEntries > 0) {
    section$Epoch <- read_section(fp, section_info$Epoch, ABF2.Epoch.def, df = FALSE)
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
    warning("No strings entries recorded.")
  }

  section
}
