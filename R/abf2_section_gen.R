gen_struct_df <- function(struct_def) {

  n <- length(struct_def$field)
  mx <- matrix(NA, nrow = 1L, ncol = n)
  colnames(mx) <- struct_def$field

  df <- data.frame(mx)
  return(df)
}

dummy_header <- function() {

  header <- list()

  header$fFileSignature <- 'ABF2'
  header$fFileVersionNumber1 <- 0
  header$fFileVersionNumber2 <- 0
  header$fFileVersionNumber3 <- 0
  header$fFileVersionNumber4 <- 0
  header$uFileInfoSize <- 512
  header$lActualEpisodes <- 0L
  header$uFileStartDate <- 0
  header$uFileStartTimeMS <- 0
  header$uStopwatchTime <- 0
  header$nFileType <- 0L
  header$nDataFormat <- 0L
  header$nSimultaneousScan <- 0L
  header$nCRCEnable <- 0
  header$uFileCRC <- 0
  header$FileGUID1 <- 0L
  header$FileGUID2 <- 0L
  header$uCreatorVersion <- 0
  #strings section
  header$uCreatorNameIndex <- 1
  header$uModifierVersion <- 0
  header$uModifierNameIndex <- 0
  #strings section
  header$uProtocolPathIndex <- 2

  header
}

dummy_protocol <- function(nOperationMode = 5L,
                           fADCSequenceInterval = 100,
                           lNumSamplesPerEpisode,
                           lEpisodesPerRun) {

  protocol <- list()

  protocol$nOperationMode <- nOperationMode
  protocol$fADCSequenceInterval <- fADCSequenceInterval
  protocol$bEnableFileCompression <- 0
  protocol$sUnused1 <- 0
  protocol$uFileCompressionRatio <- 0
  #set synch array unit to fADCSequenceInterval
  protocol$fSynchTimeUnit <- 1
  protocol$fSecondsPerRun <- 0
  protocol$lNumSamplesPerEpisode <- lNumSamplesPerEpisode
  protocol$lPreTriggerSamples <- 0
  protocol$lEpisodesPerRun <- lEpisodesPerRun
  protocol$lRunsPerTrial <- 1
  protocol$lNumberOfTrials <- 1
  protocol$nAveragingMode <- 0
  protocol$nUndoRunCount <- 0
  protocol$nFirstEpisodeInRun <- 0
  protocol$fTriggerThreshold <- 0
  protocol$nTriggerSource <- 0
  protocol$nTriggerAction <- 0
  protocol$nTriggerPolarity <- 0
  protocol$fScopeOutputInterval <- 0
  protocol$fEpisodeStartToStart <- 0
  protocol$fRunStartToStart <- 0
  protocol$lAverageCount <- 0
  protocol$fTrialStartToStart <- 0
  protocol$nAutoTriggerStrategy <- 0
  protocol$fFirstRunDelayS <- 0
  protocol$nChannelStatsStrategy <- 0
  protocol$lSamplesPerTrace <- 0
  protocol$lStartDisplayNum <- 0
  protocol$lFinishDisplayNum <- 0
  protocol$nShowPNRawData <- 0
  protocol$fStatisticsPeriod <- 0
  protocol$lStatisticsMeasurements <- 0
  protocol$nStatisticsSaveStrategy <- 0
  #do not scale
  protocol$fADCRange <- 1.0
  protocol$fDACRange <- 1.0
  protocol$lADCResolution <- 1.0
  protocol$lDACResolution <- 1.0
  protocol$nExperimentType <- 0
  protocol$nManualInfoStrategy <- 0
  protocol$nCommentsEnable <- 0
  protocol$lFileCommentIndex <- 0
  protocol$nAutoAnalyseEnable <- 0
  protocol$nSignalType <- 0
  protocol$nDigitalEnable <- 0
  protocol$nActiveDACChannel <- 0
  protocol$nDigitalHolding <- 0
  protocol$nDigitalInterEpisode <- 0
  protocol$nDigitalDACChannel <- 0
  protocol$nDigitalTrainActiveLogic <- 0
  protocol$nStatsEnable <- 0
  protocol$nStatisticsClearStrategy <- 0
  #? 1/64 npts hysteresis?
  protocol$nLevelHysteresis <- 64
  protocol$lTimeHysteresis <- 1
  protocol$nAllowExternalTags <- 0
  protocol$nAverageAlgorithm <- 0
  protocol$fAverageWeighting <- 0
  protocol$nUndoPromptStrategy <- 0
  protocol$nTrialTriggerSource <- 0
  protocol$nStatisticsDisplayStrategy <- 0
  protocol$nExternalTagType <- 0
  protocol$nScopeTriggerOut <- 0
  protocol$nLTPType <- 0
  protocol$nAlternateDACOutputState <- 0
  protocol$nAlternateDigitalOutputState <- 0
  protocol$fCellID1 <- 0
  protocol$fCellID2 <- 0
  protocol$fCellID3 <- 0
  #these are actually not used by abftools but kept for internal debugging
  protocol$nDigitizerADCs <- 16
  protocol$nDigitizerDACs <- 4
  protocol$nDigitizerTotalDigitalOuts <- 16
  protocol$nDigitizerSynchDigitalOuts <- 8
  protocol$nDigitizerType <- 6
  protocol$sUnused <- 0

  protocol
}

#ADC section is stored as a data.frame, this returns an entry/row of an ADC record/channel
dummy_adc_entry <- function(nADCNum, lADCChannelNameIndex, lADCUnitsIndex) {

  ADC <- list()

  ADC$nADCNum <- nADCNum
  ADC$nTelegraphEnable <- 0
  ADC$nTelegraphInstrument <-  0
  ADC$fTelegraphAdditGain <- 1.0
  ADC$fTelegraphFilter <- 0
  ADC$fTelegraphMembraneCap <- 0
  ADC$nTelegraphMode <- 0
  ADC$fTelegraphAccessResistance <- 0
  ADC$nADCPtoLChannelMap <- 0
  ADC$nADCSamplingSeq <- 0
  ADC$fADCProgrammableGain <- 1.0
  ADC$fADCDisplayAmplification <- 1.0
  ADC$fADCDisplayOffset <-  0
  ADC$fInstrumentScaleFactor <- 1.0
  ADC$fInstrumentOffset <- 0.0
  ADC$fSignalGain <- 1.0
  ADC$fSignalOffset <- 0.0
  ADC$fSignalLowpassFilter <- 0
  ADC$fSignalHighpassFilter <- 0
  ADC$nLowpassFilterType <- 0
  ADC$nHighpassFilterType <- 0
  ADC$fPostProcessLowpassFilter <- 0
  ADC$nPostProcessLowpassFilterType <- 0
  ADC$bEnabledDuringPN <- 0
  ADC$nStatsChannelPolarity <- 0
  ADC$lADCChannelNameIndex <- lADCChannelNameIndex
  ADC$lADCUnitsIndex <- lADCUnitsIndex
  ADC$sUnused <- 0

  ADC
}

#assumes only one dac channel
dummy_dac_entry <- function(lDACChannelNameIndex, lDACChannelUnitsIndex, nWaveformEnable) {

  DAC <- list()

  DAC$nDACNum <- 0
  DAC$nTelegraphDACScaleFactorEnable <- 0
  DAC$fInstrumentHoldingLevel <- 0
  DAC$fDACScaleFactor <- 1.0
  DAC$fDACHoldingLevel <- 0
  DAC$fDACCalibrationFactor <- 0
  DAC$fDACCalibrationOffset <- 0
  DAC$lDACChannelNameIndex <- lDACChannelNameIndex
  DAC$lDACChannelUnitsIndex <- lDACChannelUnitsIndex
  DAC$lDACFilePtr <- 0
  DAC$lDACFileNumEpisodes <- 0
  DAC$nWaveformEnable <- ifelse(as.logical(nWaveformEnable), 1L, 0L)
  #1 is the only supported source at the moment
  DAC$nWaveformSource <- 1
  DAC$nInterEpisodeLevel <- 0
  DAC$fDACFileScale <- 1.0
  DAC$fDACFileOffset <- 0
  DAC$lDACFileEpisodeNum <- 0
  DAC$nDACFileADCNum <- 0
  DAC$nConditEnable <- 0
  DAC$lConditNumPulses <- 0
  DAC$fBaselineDuration <- 0
  DAC$fBaselineLevel <- 0
  DAC$fStepDuration <- 0
  DAC$fStepLevel <- 0
  DAC$fPostTrainPeriod <- 0
  DAC$fPostTrainLevel <- 0
  DAC$nMembTestEnable <- 0
  DAC$nLeakSubtractType <- 0
  DAC$nPNPolarity <- 0
  DAC$fPNHoldingLevel <- 0
  DAC$nPNNumADCChannels <- 0
  DAC$nPNPosition <- 0
  DAC$nPNNumPulses <- 0
  DAC$fPNSettlingTime <- 0
  DAC$fPNInterpulse <- 0
  DAC$nLTPUsageOfDAC <- 0
  DAC$nLTPPresynapticPulses <- 0
  DAC$lDACFilePathIndex <- 0
  DAC$fMembTestPreSettlingTimeMS <- 0
  DAC$fMembTestPostSettlingTimeMS <- 0
  DAC$nLeakSubtractADCIndex <- 0
  DAC$sUnused <- 0

  DAC
}

dummy_epoch_per_dac <- function(num_epochs) {

  mx <- matrix(NA, nrow = num_epochs, ncol = 10L)
  epdac <- data.frame(mx)
  colnames(epdac) <- c("nEpochNum",
                    "nDACNum",
                    "nEpochType",
                    "fEpochInitLevel",
                    "fEpochLevelInc",
                    "lEpochInitDuration",
                    "lEpochDurationInc",
                    "lEpochPulsePeriod",
                    "lEpochPulseWidth",
                    "sUnused")
  epdac$nEpochNum <- seq_len(num_epochs) - 1L
  epdac$nDACNum <- rep(0L, num_epochs)
  epdac$nEpochType <- rep(1L, num_epochs)
  epdac$fEpochInitLevel <- rep(0, num_epochs)
  epdac$fEpochLevelInc <- rep(0, num_epochs)
  epdac$lEpochInitDuration <- rep(0, num_epochs)
  epdac$lEpochDurationInc <- rep(0, num_epochs)
  epdac$lEpochPulsePeriod <- rep(0, num_epochs)
  epdac$lEpochPulseWidth <- rep(0, num_epochs)
  epdac$sUnused <- rep(0, num_epochs)

  epdac
}

dummy_strings <- function(num_entries) {

  strings <- vector(mode = "list", length = num_entries)
  strings[[1]] <- "abftools"
  strings[[2]] <- "abftools"

  strings
}
