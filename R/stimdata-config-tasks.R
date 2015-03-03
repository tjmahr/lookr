
GetDialectCode <- function(filename) {
  # Look for subject identifier substring
  sub_id <- str_extract(filename, lwl_constants$l2t_subject)
  # Pull the second to last character
  code <- str_sub(sub_id, nchar(sub_id) - 1, nchar(sub_id) - 1)
  ifelse(code == "S", "SAE", "AAE")
}




#### Frankenfunction to preconfigure the various RWL, MP, Coartic tasks

# Get the names of the protocol-related stimdata
#
# A cheatsheet describing the stimdata fields by protocol and experiment is
# provided in a file called `stimdata_by_task_and_protocol_cheatsheet.xls`.
#
# @param protocol_name a string specifying the name of the eye-tracking
#   protocol used in the experiment. As of Jan 2013, these are: `WFF_Movie`,
#   `WFF_Area` and `NoFixations`.
# @return A vector of named character strings specifying the names of the
#   stimdata fields used in the protocol
#



# #### Adding timing attributes based on experimental protocol

# In the No Fixations protocol, the carrier phrase and the target word are
# contained in one audio file, and what we know about the timing in the audio
# file to determine the onset and end of the target word. In the Wait For
# Fixation protocols, the target word is played separately and has stim values
# recording the duration and onset time of the target. Moreover, there may be
# some amount of delay between the end of the carrier phrase and the onset of
# the target word, but these are recorded in the stimlog file.

DetermineProtocol <- function (stimlog) {
  time <- .CheckForStimdataType(stimlog, 'Fixation.OnsetTime')
  movs <- .CheckForStimdataType(stimlog, 'FixationMovie')
  protocol <- if (movs) "WFF_Movie" else if (time) "WFF_Area" else "NoFixations"
  protocol
}

# Get the appropriate stim from the protocol name.
ConfigureProtocol <- function(protocol, stimlog) {
  # We list the stim for the three protocols and choose the appropriate set.

  # Stim shared across all protocols in all experiments.
  shared <- c(Target = 'Target',
              WordGroup = 'WordGroup',
              StimType = 'StimType',
              ImageOnset = 'Image2sec.OnsetTime',
              # CarrierDur = 'CarrierDur',
              Audio = 'AudioStim',
              Attention = 'Attention',
              AttentionDur = 'AttentionDur')

  # Stim shared by the two Wait-For-Fixation protocols.
  fixation_shared <- c(CarrierOnset = 'CarrierStim.OnsetTime',
                       FixationOnset = 'Fixation.OnsetTime',
                       DelayTargetOnset = 'Target.OnsetDelay',
                       TargetOnset = 'Target.OnsetTime',
                       TargetDur = 'AudioDur',
                       CarrierDur = 'CarrierDur')

  # Make the stim-lists for each protocol. The `NoFixations` and `WFF_Movie`
  # protocols have an extra stimdata field unique to each one.
  no_fixation <- c(shared, CarrierOnset = 'AudioStim.OnsetTime')
  wff_area    <- c(shared, fixation_shared)
  wff_movie   <- c(shared, fixation_shared, FixationMovie = 'FixationMovie')

  # Choose the stim based on protocol name
  stim_set <- switch(protocol,
                     "WFF_Movie" = wff_movie,
                     "WFF_Area" = wff_area,
                     "NoFixations" = no_fixation)


  # Some experiments include an Attention.Onset field. (noticed Mar. 2015)
  attention_onset <- .CheckForStimdataType(stimlog, 'Attention.OnsetTime')

  if (attention_onset) {
    stim_set <- c(stim_set, AttentionOnset = 'Attention.OnsetTime')
  }

  # Make a stimdata configuration description with the available stimdata info
  config <- MakeStimdataConfig(stim_set)
  num_stim <- c('AttentionDur', 'CarrierDur', 'TargetDur', 'ImageOnset',
                'FixationOnset', 'CarrierOnset', 'DelayTargetOnset',
                'TargetOnset', 'AttentionOnset')
  current_num_stim <- num_stim[num_stim %in% names(config$Stim)]
  config <- AddNumerics(config, current_num_stim)
  config <- AddConstants(config, c(Protocol = protocol))

  # NoFixations: There is one audio file. We need to derived TargetOnset.
  if (protocol == "NoFixations") {
    # The "CarrierDur" and "TargetDur" constants are set later in the
    # DetermineStim function
    config <- AddNumericConstant(config, c(DelayTargetOnset = 0))
    derived <- c("TargetOnset <- CarrierOnset + CarrierDur",
                 "TargetEnd <- TargetOnset + TargetDur",
                 "CarrierEnd <- CarrierOnset + CarrierDur",
                 # Attention-getter starts ~1000 ms after TargetEnd
                 "AttentionOnset <- TargetEnd + 1000",
                 "AttentionEnd <- AttentionOnset + AttentionDur")
    if (attention_onset) derived <- derived[-4]
    config <- AddDerived(config, derived)
  }

  # Only AttentionOnset needs to be hard-coded for the WFF protocols
  if (protocol == "WFF_Area" | protocol == "WFF_Movie") {
    derived <- c("TargetEnd <- TargetOnset + TargetDur",
                 "CarrierEnd <- CarrierOnset + CarrierDur",
                 "FixationDur <- CarrierOnset - FixationOnset",
                 # Attention-getter starts ~1000 ms after TargetEnd
                 "AttentionOnset <- TargetEnd + 1000",
                 "AttentionEnd <- AttentionOnset + AttentionDur")
    if (attention_onset) derived <- derived[-4]
    config <- AddDerived(config, derived)
  }

  # Some experiments no longer have attention-getters (effective Nov. 2014).
  # Remove them from the config if they are not referenced in stimlog
  attention <- .CheckForStimdataType(stimlog, 'Attention')
  if (!attention) {
    remove_attention <- function(xs) xs[!str_detect(xs, "Attention")]
    config$Stim <- remove_attention(config$Stim)
    config$Numerics <- remove_attention(config$Numerics)
    config$Constants <- remove_attention(config$Constants)
    config$Derived <- remove_attention(config$Derived)
  }

  config
}




DetermineStim <- function(stimlog) UseMethod("DetermineStim")

# Throw an error if the Task is name is invalid.
DetermineStim.default <- function(stimlog) {
  error_names <- paste0("\"", class(stimlog), "\"", collapse=", ")
  stop(paste0("Invalid task name in ", error_names))
}

DetermineStim.MP <- function(stimlog) {
  # Initialize the stim configuration from the protocol information
  config <- ConfigureProtocol(DetermineProtocol(stimlog), stimlog)

  # Load the specific stimuli for this experiment
  config <- AddStim(config, c(ImageL = "ImageL", ImageR = "ImageR"))

  # Add Dialect, Task, Subject, Block constants
  basename <- attr(stimlog, "Basename")
  dialect <- GetDialectCode(basename)
  file_info <- ParseFilename(basename)
  string_constants <- c(Dialect = dialect, Task = file_info$Task,
                        Subject = file_info$Subject)

  config <- AddConstants(config, string_constants)
  config <- AddNumericConstant(config, c(Block = file_info$Block))

  # Configure timing details
  protocol <- config$Constants["Protocol"]

  # NoFixations: There is one audio file. We need to derived TargetOnset.
  if (protocol == "NoFixations") {
    # The SAE stim has variable target durations:

    #      dog/tog/veif: 743 ms.
    #      toes/doz/fId: 884 ms.
    #     shoes/suz/giv: 927 ms.
    #      soup/Sup/Cim: 917 ms.
    #     duck/guck/San: 669 ms.
    #     girl/dirl/neJ: 779 ms.

    # But the CarrierDuration is hardcoded in the stimdata file as 2307. We use
    # the difference between that value and the actual carrier duration (1496)
    # as the derived target durations: 2307 - 1496 = 811.

    carrier_duration <- ifelse(dialect == "SAE", 1496, 1149)
    target_duration <- ifelse(dialect == "SAE", 811, 708)
    config <- AddNumericConstant(config, c(CarrierDur = carrier_duration,
                                           TargetDur = target_duration))
  }

  # Print a helpful message
  task <- config$Constants["Task"]
  message(paste0("Task: ", task, ",  Protocol: ", protocol,
                 ",  Dialect: ", dialect))

  config
}




DetermineStim.RWL <- function(stimlog) {
  # Initialize the stim configuration from the protocol information
  config <- ConfigureProtocol(DetermineProtocol(stimlog), stimlog)

  # Load the specific stimuli for this experiment
  rwl_stim <- c(UpperLeftImage  = 'Image1', UpperRightImage = 'Image2',
                LowerRightImage = 'Image3', LowerLeftImage  = 'Image4',
                SemanticFoil = 'SemanticFoil',
                PhonologicalFoil = 'PhonologicalFoil',
                Unrelated = 'Unrelated')
  config <- AddStim(config, rwl_stim)

  # Add Dialect, Task, Subject, Block constants
  basename <- attr(stimlog, "Basename")
  dialect <- GetDialectCode(basename)
  file_info <- ParseFilename(basename)

  string_constants <- c(Dialect = dialect, Task = file_info$Task,
                        Subject = file_info$Subject)

  config <- AddConstants(config, string_constants)
  config <- AddNumericConstant(config, c(Block = file_info$Block))

  ## Configure timing details

  protocol <- config$Constants["Protocol"]

  # NoFixations: There is one audio file. We need to derived TargetOnset.
  if (protocol == "NoFixations") {
    carrier_duration <- ifelse(dialect == "SAE", 1496, 1149)
    target_duration <- ifelse(dialect == "SAE", 845, 706)
    config <- AddNumericConstant(config, c(CarrierDur = carrier_duration,
                                           TargetDur = target_duration))
  }

  # Print a helpful message
  task <- config$Constants["Task"]
  message(paste0("Task: ", task, ",  Protocol: ", protocol,
                 ",  Dialect: ", dialect))

  config
}



DetermineStim.Coartic <- function(stimlog) {
  shared <- c(ImageL = "ImageL",
              ImageR = "ImageR",
              Carrier = "Carrier",
              TargetImage = "Target",
              Target = "TargetWord",
              Audio = "AudioStim",
              Attention = "Attention",
              WordGroup = "WordGroup",
              StimType = "StimType",
              TargetWord = "TargetWord")

  shared_num <- c(AttentionDur = 'AttentionDur',
                  ImageOnset = 'Image2sec.OnsetTime',
                  FixationOnset = 'Fixation.OnsetTime')

  v1_num <- c(CarrierOnset = 'CarrierStim.OnsetTime',
              CarrierDur = 'CarrierDur',
              DelayTargetOnset = 'Target.OnsetDelay',
              TargetOnset = 'Target.OnsetTime',
              TargetDur = 'AudioDur')
  v1_numerics <- c(names(shared_num), names(v1_num))
  v1_stim <- c(shared, shared_num, v1_num)
  v1_derived <- c("FixationDur <- CarrierOnset - FixationOnset",
                  "CarrierEnd <- CarrierOnset + CarrierDur",
                  "TargetEnd <- TargetOnset + TargetDur",
                  "AttentionOnset <- TargetEnd + 1000",
                  "AttentionEnd <- AttentionOnset + AttentionDur")
  v1_config <- MakeStimdataConfig(v1_stim, v1_numerics, NULL, v1_derived)

  v2_constants <- c(CarrierDur = 1340, TargetDur = 850, DelayTargetOnset = 0)
  v2_num <- c(DelayCarrierOnset = "Target.OnsetDelay",
              CarrierOnset = "Target.OnsetTime",
              AttentionOnset = "Attention.OnsetTime")
  v2_numerics <- c(names(shared_num), names(v2_num), names(v2_constants))
  v2_stim <- c(shared, shared_num, v2_num, Pitch = "Pitch")

  v2_derived <- c("FixationDur <- CarrierOnset - FixationOnset",
                  "TargetOnset <- CarrierOnset + CarrierDur",
                  "CarrierEnd <- CarrierOnset + CarrierDur",
                  "TargetEnd <- TargetOnset + TargetDur",
                  "AttentionEnd <- AttentionOnset + AttentionDur")

  v2_config <- MakeStimdataConfig(v2_stim, v2_numerics, v2_constants, v2_derived)


  version_2 <- .CheckForStimdataType(stimlog, 'Pitch')

  config <- if (version_2) v2_config else v1_config


  # Add Dialect, Task, Subject, Block constants
  basename <- attr(stimlog, "Basename")

  file_info <- ParseFilename(basename)
  string_constants <- c(Task = file_info$Task, Subject = file_info$Subject, Protocol = "WFF_Area")

  config <- AddConstants(config, string_constants)
  config <- AddNumericConstant(config, c(Block = file_info$Block))

  config

}



