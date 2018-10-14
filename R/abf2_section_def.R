# Block size of ABF file
ABF2.BlockSize <- 512L
ABF2.StringOffset <- 45L

# Definition of ABF2 header
ABF2.Header.field <- c("fFileSignature", #0
                       "fFileVersionNumber1", #4
                       "fFileVersionNumber2", #5
                       "fFileVersionNumber3", #6
                       "fFileVersionNumber4", #7
                       "uFileInfoSize", #8
                       "lActualEpisodes", #12
                       "uFileStartDate", #16
                       "uFileStartTimeMS", #20
                       "uStopwatchTime", #24
                       "nFileType", #28
                       "nDataFormat", #30
                       "nSimultaneousScan", #32
                       "nCRCEnable", #34
                       "uFileCRC", #36
                       "FileGUID1", #40
                       "FileGUID2", #48
                       "uCreatorVersion", #56
                       "uCreatorNameIndex", #60
                       "uModifierVersion", #64
                       "uModifierNameIndex", #68
                       "uProtocolPathIndex") #72
ABF2.Header.ctype <- c("string", "int8", "int8", "int8", "int8", "uint32",
                       "uint32", "uint32", "uint32", "uint32", "int16", "int16",
                       "int16", "int16", "uint32", "int64", "int64", "uint32",
                       "uint32", "uint32", "uint32", "uint32")
ABF2.Header.ssize <- c(4L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                       0L, 0L, 0L, 0L, 0L, 0L, 0L)
ABF2.Header.def <- list()
ABF2.Header.def$field <- ABF2.Header.field
ABF2.Header.def$ctype <- ABF2.Header.ctype
ABF2.Header.def$ssize <- ABF2.Header.ssize

# Definition of section info
ABF2.SectionInfo.field <- c("uBlockIndex", "uBytes", "llNumEntries")
ABF2.SectionInfo.ctype <- c("uint32", "uint32", "int64")
ABF2.SectionInfo.ssize <- c(0L, 0L, 0L)
ABF2.SectionInfo.def <- list()
ABF2.SectionInfo.def$field <- ABF2.SectionInfo.field
ABF2.SectionInfo.def$ctype <- ABF2.SectionInfo.ctype
ABF2.SectionInfo.def$ssize <- ABF2.SectionInfo.ssize

ABF2.SectionInfoList <- c("Protocol",
                          "ADC",
                          "DAC",
                          "Epoch",
                          "ADCPerDAC",
                          "EpochPerDAC",
                          "UserList",
                          "StatsRegion",
                          "Math",
                          "Strings",
                          "Data",
                          "Tag",
                          "Scope",
                          "Delta",
                          "VoiceTag",
                          "SynchArray",
                          "Annotation",
                          "Stats")

# Sections
ABF2.Protocol.field <- c( "nOperationMode",
                          "fADCSequenceInterval",
                          "bEnableFileCompression",
                          "sUnused1",
                          "uFileCompressionRatio",
                          "fSynchTimeUnit",
                          "fSecondsPerRun",
                          "lNumSamplesPerEpisode",
                          "lPreTriggerSamples",
                          "lEpisodesPerRun",
                          "lRunsPerTrial",
                          "lNumberOfTrials",
                          "nAveragingMode",
                          "nUndoRunCount",
                          "nFirstEpisodeInRun",
                          "fTriggerThreshold",
                          "nTriggerSource",
                          "nTriggerAction",
                          "nTriggerPolarity",
                          "fScopeOutputInterval",
                          "fEpisodeStartToStart",
                          "fRunStartToStart",
                          "lAverageCount",
                          "fTrialStartToStart",
                          "nAutoTriggerStrategy",
                          "fFirstRunDelayS",
                          "nChannelStatsStrategy",
                          "lSamplesPerTrace",
                          "lStartDisplayNum",
                          "lFinishDisplayNum",
                          "nShowPNRawData",
                          "fStatisticsPeriod",
                          "lStatisticsMeasurements",
                          "nStatisticsSaveStrategy",
                          "fADCRange",
                          "fDACRange",
                          "lADCResolution",
                          "lDACResolution",
                          "nExperimentType",
                          "nManualInfoStrategy",
                          "nCommentsEnable",
                          "lFileCommentIndex",
                          "nAutoAnalyseEnable",
                          "nSignalType",
                          "nDigitalEnable",
                          "nActiveDACChannel",
                          "nDigitalHolding",
                          "nDigitalInterEpisode",
                          "nDigitalDACChannel",
                          "nDigitalTrainActiveLogic",
                          "nStatsEnable",
                          "nStatisticsClearStrategy",
                          "nLevelHysteresis",
                          "lTimeHysteresis",
                          "nAllowExternalTags",
                          "nAverageAlgorithm",
                          "fAverageWeighting",
                          "nUndoPromptStrategy",
                          "nTrialTriggerSource",
                          "nStatisticsDisplayStrategy",
                          "nExternalTagType",
                          "nScopeTriggerOut",
                          "nLTPType",
                          "nAlternateDACOutputState",
                          "nAlternateDigitalOutputState",
                          "fCellID1",
                          "fCellID2",
                          "fCellID3",
                          "nDigitizerADCs",
                          "nDigitizerDACs",
                          "nDigitizerTotalDigitalOuts",
                          "nDigitizerSynchDigitalOuts",
                          "nDigitizerType",
                          "sUnused")
ABF2.Protocol.ctype <- c("int16", "float", "int8", "unused", "uint32", "float",
                         "float", "int32", "int32", "int32", "int32", "int32",
                         "int16", "int16", "int16", "float", "int16", "int16",
                         "int16", "float", "float", "float", "int32", "float",
                         "int16", "float", "int16", "int32", "int32", "int32",
                         "int16", "float", "int32", "int16", "float", "float",
                         "int32", "int32", "int16", "int16", "int16", "int32",
                         "int16", "int16", "int16", "int16", "int16", "int16",
                         "int16", "int16", "int16", "int16", "int16", "int32",
                         "int16", "int16", "float", "int16", "int16", "int16",
                         "int16", "int16", "int16", "int16", "int16", "float",
                         "float", "float", "int16", "int16", "int16", "int16",
                         "int16", "unused"  )
ABF2.Protocol.ssize <- c(0L, 0L, 0L, 3L,
                         0L, 0L, 0L, 0L,
                         0L, 0L, 0L, 0L,
                         0L, 0L, 0L, 0L,
                         0L, 0L, 0L, 0L,
                         0L, 0L, 0L, 0L,
                         0L, 0L, 0L, 0L,
                         0L, 0L, 0L, 0L,
                         0L, 0L, 0L, 0L,
                         0L, 0L, 0L, 0L,
                         0L, 0L, 0L, 0L,
                         0L, 0L, 0L, 0L,
                         0L, 0L, 0L, 0L,
                         0L, 0L, 0L, 0L,
                         0L, 0L, 0L, 0L,
                         0L, 0L, 0L, 0L,
                         0L, 0L, 0L, 0L,
                         0L, 0L, 0L, 0L,
                         0L, 304L)
ABF2.Protocol.def <- list()
ABF2.Protocol.def$field <- ABF2.Protocol.field
ABF2.Protocol.def$ctype <- ABF2.Protocol.ctype
ABF2.Protocol.def$ssize <- ABF2.Protocol.ssize

ABF2.ADC.field <- c("nADCNum",
                    "nTelegraphEnable",
                    "nTelegraphInstrument",
                    "fTelegraphAdditGain",
                    "fTelegraphFilter",
                    "fTelegraphMembraneCap",
                    "nTelegraphMode",
                    "fTelegraphAccessResistance",
                    "nADCPtoLChannelMap",
                    "nADCSamplingSeq",
                    "fADCProgrammableGain",
                    "fADCDisplayAmplification",
                    "fADCDisplayOffset",
                    "fInstrumentScaleFactor",
                    "fInstrumentOffset",
                    "fSignalGain",
                    "fSignalOffset",
                    "fSignalLowpassFilter",
                    "fSignalHighpassFilter",
                    "nLowpassFilterType",
                    "nHighpassFilterType",
                    "fPostProcessLowpassFilter",
                    "nPostProcessLowpassFilterType",
                    "bEnabledDuringPN",
                    "nStatsChannelPolarity",
                    "lADCChannelNameIndex",
                    "lADCUnitsIndex",
                    "sUnused")
ABF2.ADC.ctype <- c("int16", "int16", "int16", "float", "float", "float",
                    "int16", "float", "int16", "int16", "float", "float",
                    "float", "float", "float", "float", "float", "float",
                    "float", "int8", "int8", "float", "int8", "int8", "int16",
                    "int32", "int32", "unused")
ABF2.ADC.ssize <- c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 46L)
ABF2.ADC.def <- list()
ABF2.ADC.def$field <- ABF2.ADC.field
ABF2.ADC.def$ctype <- ABF2.ADC.ctype
ABF2.ADC.def$ssize <- ABF2.ADC.ssize

ABF2.DAC.field <- c("nDACNum",
                    "nTelegraphDACScaleFactorEnable",
                    "fInstrumentHoldingLevel",
                    "fDACScaleFactor",
                    "fDACHoldingLevel",
                    "fDACCalibrationFactor",
                    "fDACCalibrationOffset",
                    "lDACChannelNameIndex",
                    "lDACChannelUnitsIndex",
                    "lDACFilePtr",
                    "lDACFileNumEpisodes",
                    "nWaveformEnable",
                    "nWaveformSource",
                    "nInterEpisodeLevel",
                    "fDACFileScale",
                    "fDACFileOffset",
                    "lDACFileEpisodeNum",
                    "nDACFileADCNum",
                    "nConditEnable",
                    "lConditNumPulses",
                    "fBaselineDuration",
                    "fBaselineLevel",
                    "fStepDuration",
                    "fStepLevel",
                    "fPostTrainPeriod",
                    "fPostTrainLevel",
                    "nMembTestEnable",
                    "nLeakSubtractType",
                    "nPNPolarity",
                    "fPNHoldingLevel",
                    "nPNNumADCChannels",
                    "nPNPosition",
                    "nPNNumPulses",
                    "fPNSettlingTime",
                    "fPNInterpulse",
                    "nLTPUsageOfDAC",
                    "nLTPPresynapticPulses",
                    "lDACFilePathIndex",
                    "fMembTestPreSettlingTimeMS",
                    "fMembTestPostSettlingTimeMS",
                    "nLeakSubtractADCIndex",
                    "sUnused")
ABF2.DAC.ctype <- c("int16", "int16", "float", "float", "float", "float",
                    "float", "int32", "int32", "int32", "int32", "int16",
                    "int16", "int16", "float", "float", "int32", "int16",
                    "int16", "int32", "float", "float", "float", "float",
                    "float", "float", "int16", "int16", "int16", "float",
                    "int16", "int16", "int16", "float", "float", "int16",
                    "int16", "int32", "float", "float", "int16", "unused")
ABF2.DAC.ssize <- c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 124L)
ABF2.DAC.def <- list()
ABF2.DAC.def$field <- ABF2.DAC.field
ABF2.DAC.def$ctype <- ABF2.DAC.ctype
ABF2.DAC.def$ssize <- ABF2.DAC.ssize

ABF2.Epoch.field <- c("nEpochNum",
                      "nDigitalValue",
                      "nDigitalTrainValue",
                      "nAlternateDigitalValue",
                      "nAlternateDigitalTrainValue",
                      "bEpochCompression",
                      "sUnused")
ABF2.Epoch.ctype <- c("int16", "int16", "int16", "int16", "int16", "int8",
                      "unused")
ABF2.Epoch.ssize <- c(0L, 0L, 0L, 0L, 0L, 0L, 21L)
ABF2.Epoch.def <- list()
ABF2.Epoch.def$field <- ABF2.Epoch.field
ABF2.Epoch.def$ctype <- ABF2.Epoch.ctype
ABF2.Epoch.def$ssize <- ABF2.Epoch.ssize

#Structure not clear
ABF2.ADCPerDAC.field <- c()
ABF2.ADCPerDAC.ctype <- c()
ABF2.ADCPerDAC.ssize <- c()
ABF2.ADCPerDAC.def <- list()
ABF2.ADCPerDAC.def$field <- ABF2.ADCPerDAC.field
ABF2.ADCPerDAC.def$ctype <- ABF2.ADCPerDAC.ctype
ABF2.ADCPerDAC.def$ssize <- ABF2.ADCPerDAC.ssize

ABF2.EpochPerDAC.field <- c("nEpochNum",
                            "nDACNum",
                            "nEpochType",
                            "fEpochInitLevel",
                            "fEpochLevelInc",
                            "lEpochInitDuration",
                            "lEpochDurationInc",
                            "lEpochPulsePeriod",
                            "lEpochPulseWidth",
                            "sUnused")
ABF2.EpochPerDAC.ctype <- c( "int16", "int16", "int16", "float", "float",
                             "int32", "int32", "int32", "int32", "unused")
ABF2.EpochPerDAC.ssize <- c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 18L)
ABF2.EpochPerDAC.def <- list()
ABF2.EpochPerDAC.def$field <- ABF2.EpochPerDAC.field
ABF2.EpochPerDAC.def$ctype <- ABF2.EpochPerDAC.ctype
ABF2.EpochPerDAC.def$ssize <- ABF2.EpochPerDAC.ssize

ABF2.UserList.field <- c("nListNum",
                         "nULEnable",
                         "nULParamToVary",
                         "nULRepeat",
                         "lULParamValueListIndex",
                         "sUnused")
ABF2.UserList.ctype <- c("int16", "int16", "int16", "int16", "int32", "unused")
ABF2.UserList.ssize <- c(0L, 0L, 0L, 0L, 0L, 52L)
ABF2.UserList.def <- list()
ABF2.UserList.def$field <- ABF2.UserList.field
ABF2.UserList.def$ctype <- ABF2.UserList.ctype
ABF2.UserList.def$ssize <- ABF2.UserList.ssize

ABF2.StatsRegion.field <- c("nRegionNum",
                            "nADCNum",
                            "nStatsActiveChannels",
                            "nStatsSearchRegionFlags",
                            "nStatsSelectedRegion",
                            "nStatsSmoothing",
                            "nStatsSmoothingEnable",
                            "nStatsBaseline",
                            "lStatsBaselineStart",
                            "lStatsBaselineEnd",
                            "lStatsMeasurements",
                            "lStatsStart",
                            "lStatsEnd",
                            "nRiseBottomPercentile",
                            "nRiseTopPercentile",
                            "nDecayBottomPercentile",
                            "nDecayTopPercentile",
                            "nStatsSearchMode",
                            "nStatsSearchDAC",
                            "nStatsBaselineDAC",
                            "sUnused")
ABF2.StatsRegion.ctype <- c("int16", "int16", "int16", "int16", "int16",
                            "int16", "int16", "int16", "int32", "int32",
                            "int32", "int32", "int32", "int16", "int16",
                            "int16", "int16", "int16", "int16", "int16",
                            "unused")
ABF2.StatsRegion.ssize <- c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                            0L, 0L, 0L, 0L, 0L, 0L, 0L, 78L)
ABF2.StatsRegion.def <- list()
ABF2.StatsRegion.def$field <- ABF2.StatsRegion.field
ABF2.StatsRegion.def$ctype <- ABF2.StatsRegion.ctype
ABF2.StatsRegion.def$ssize <- ABF2.StatsRegion.ssize

ABF2.Math.field <- c("nMathEnable", "nMathExpression", "uMathOperatorIndex",
                     "uMathUnitsIndex", "fMathUpperLimit", "fMathLowerLimit",
                     "nMathADCNum1", "nMathADCNum2", "sUnused1", "fMathK1",
                     "fMathK2", "fMathK3", "fMathK4", "fMathK5", "fMathK6",
                     "sUnused2")
ABF2.Math.ctype <- c("int16", "int16", "uint32", "uint32",
                     "float", "float", "int16", "int16",
                     "unused", "float", "float", "float",
                     "float", "float", "float", "unused")
ABF2.Math.ssize <- c(0L, 0L, 0L, 0L,
                     0L, 0L, 0L, 0L,
                     16L, 0L, 0L, 0L,
                     0L, 0L, 0L, 64L)
ABF2.Math.def <- list()
ABF2.Math.def$field <- ABF2.Math.field
ABF2.Math.def$ctype <- ABF2.Math.ctype
ABF2.Math.def$ssize <- ABF2.Math.ssize

#Header sections from ABF1
#Data not padded
ABF2.Tag.field <- c("lTagTime", "sComment", "nTagType",
                    "nVoiceTagNumber_nAnnotationIndex")
ABF2.Tag.ctype <- c("int32", "string", "int16", "int16")
ABF2.Tag.ssize <- c(0L, 56L, 0L, 0L)
ABF2.Tag.def <- list()
ABF2.Tag.def$field <- ABF2.Tag.field
ABF2.Tag.def$ctype <- ABF2.Tag.ctype
ABF2.Tag.def$ssize <- ABF2.Tag.ssize

#Sections that may not needed for handling voltage clamp data
#Scope
#Delta
#VoiceTag
#Annotation
#Stats

#Synch array maybe the key to read variable-length data
ABF2.SynchArray.field <- c("lStart", "lLength")
ABF2.SynchArray.ctype <- c("int32", "int32")
ABF2.SynchArray.ssize <- c(0L, 0L)
ABF2.SynchArray.def <- list()
ABF2.SynchArray.def$field <- ABF2.SynchArray.field
ABF2.SynchArray.def$ctype <- ABF2.SynchArray.ctype
ABF2.SynchArray.def$ssize <- ABF2.SynchArray.ssize
