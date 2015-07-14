library("tools")
library("rprime")
library("lookr")
library("magrittr")
library("stringr")
library("dplyr")
library("lubridate")
source("inst/eprime_helpers.R")

# test of teleport function
df <- data.frame(
  Location = c("Left", "Right", "Right", "Left"),
  Left = c("l1", "l2", "l3", "l4"),
  Right = c("r1", "r2", "r3", "r4"),
  stringsAsFactors = FALSE)
teleport_by_column(df, "Location")
# [1] "l1" "r2" "r3" "l4"

# Try to replicate the results of stimdata for the MP task
stimpath <- "tests/testthat/data/MP_WFFArea_Long/001P00XS1/MP_Block1_001P00XS1.txt"
stimdata <- stimpath %>% read_eprime %>% FrameList

# Get non-column values
non_column_values <- parse_l2t_filename(stimpath)
non_column_values$DateTime <- get_eprime_datetime(stimdata)

# Just keep the trial level data
stimdata <- stimdata %>%
  keep_levels(level_numbers = 3) %>%
  to_data_frame %>% as.tbl

# Drop junk Eprime columns and start time columns
names_to_drop <- c("Eprime.Level", "Eprime.LevelName", "Eprime.FrameNumber",
                   "Procedure", "Running", "Cycle")

stimdata <- stimdata %>%
  select(-one_of(names_to_drop), -contains("StartTime"))

# Rename columns
names(stimdata) <- names(stimdata) %>%
  str_replace("^Sample$", "TrialNo") %>%
  str_replace("\\.OnsetDelay", "OnsetDelay") %>%
  str_replace("Image2sec", "Image") %>%
  str_replace("CarrierStim", "Carrier") %>%
  str_replace("\\.OnsetTime", "Onset") %>%
  str_replace("Eprime.Basename", "Basename") %>%
  str_replace("AudioStim", "Audio")

data_frame(
  Condition = stimdata$StimType,
  Target = teleport_by_column(stimdata, "Target"))

# Set the target to the familiar image in the MP and nonsense trials
stimdata <- set_mp_targets(stimdata)

data_frame(
  Condition = stimdata$StimType,
  Target = teleport_by_column(stimdata, "TargetImage"))

stimdata$Target <-
  teleport_by_column(stimdata, "TargetImage") %>%
  # Convert image-filename to word-label
  str_replace("\\d$", "")

# lefts <- which(stimdata$TargetImage == "ImageL")
# right <- which(stimdata$TargetImage == "ImageR")
# stimdata[lefts, "Target"]
# stimdata[right, "Target"]

# Make number-ful columns into numeric values
stimdata <- stimdata %>% lapply(try_numeric) %>% as_data_frame

# Attach single value columns
non_column_values$Protocol <- determine_protocol(stimdata)

for (x in names(non_column_values)) {
  stimdata[[x]] <- non_column_values[[x]]
}



ref_stimdata <- Stimdata(stimpath)
ref_stimdata <- ref_stimdata[sort(names(ref_stimdata))]
stimdata <- stimdata[sort(names(stimdata))]

glimpse(ref_stimdata)
str(ref_stimdata)
str(stimdata)

for (x in names(stimdata)) {
  print(x)
  check <- all(stimdata[[x]] == ref_stimdata[[x]])
  if (!check) warning(x)
}

stimdata$Target
ref_stimdata$Target



if (is.element("CarrierDur", names(stimdata))) {
  stimdata <- rename(stimdata, TargetDur = AudioDur)
}

# Add [EVENT]End columns. These are made by adding [EVENT]Onset to[Event]Dur.

# Get the [EVENT]Dur columns and possible [EVENT]Onset columns
durs <- str_filter(names(stimdata), "Dur$")
maybe_onsets <- str_replace(durs, "Dur", "Onset")

# Keep only [EVENT]Onset values that are columns in stimdata.
onsets <- maybe_onsets[is.element(maybe_onsets, names(stimdata))]
events_names <- str_replace(onsets, "Onset$", "")

# Do each EventEnd calculation.
for (event in events_names) {
  levels <- c("End", "Onset", "Dur")
  events <- structure(as.list(paste0(event, levels)), names = levels)
  stimdata[[events$End]] <- stimdata[[events$Onset]] + stimdata[[events$Dur]]
}

setdiff(names(ref_stimdata), names(stimdata))
# [1] "DelayTargetOnset" "Dialect"          "FixationDur"      "TargetWord"



# stimout <- paste0(tools::file_path_sans_ext(stimpath), ".stimdata")
# write.csv(stimdata, file = stimout, row.names = FALSE)

glimpse(stimdata)

trials[[1]] %>% attributes %>% names
# [1] "row.names"        "names"            "class"            "Task"
# [5] "Protocol"         "DateTime"         "Subject"          "Block"
# [9] "TrialNo"          "TargetWord"       "ImageL"           "ImageR"
# [13] "Target"           "DistractorImage"  "WordGroup"        "StimType"
# [17] "ImageOnset"       "Audio"            "Attention"        "AttentionDur"
# [21] "CarrierOnset"     "FixationOnset"    "DelayTargetOnset" "TargetOnset"
# [25] "TargetDur"        "CarrierDur"       "Dialect"          "AttentionEnd"
# [29] "AttentionOnset"   "FixationDur"      "CarrierEnd"       "TargetEnd"
# [33] "TargetImage"      "Basename"












gazedata <- Gazedata(paste0(tools::file_path_sans_ext(stimpath), ".gazedata"))




my_trials <- Block(gazedata = gazedata, stimdata = stimdata)


my_trials %>% AdjustTimes %>% TimeSlice(200,2000) %>% AddAOIData


lookr:::.GetTargetImage.Stimdata
lookr:::CombineGazedataStimdata(Gazedata(tools::file_path_sans_ext(stimpath)), stimdata)
CombineGazedataStimdata <- lookr:::CombineGazedataStimdata

lapply(stimdata$TrialNo, CombineGazedataStimdata(gazedata, stimdata))




lookr:::Block.Gazedata

