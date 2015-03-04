# github.com/tjmahr/rprime
library("tools")
library("rprime")
# library("lookr")
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
trials <- Session("inst/docs/data/MP_WFFArea_Long/001P00XS1/")
trials %@% "AttentionOnset" - trials %@% "TargetEnd"

trials <- Session("tests/testthat/data/MP_WFFMovie_CS2b/001P00XS2/")
trials %@% "AttentionOnset" - trials %@% "TargetEnd"

stimpath <- "inst/docs/data/MP_WFFArea_Long/001P00XS1/MP_Block1_001P00XS1.txt"
stimpath <- "tests/testthat/data/MP_WFFMovie_CS2b/001P00XS2/MP_Block1_001P00XS2.txt"
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

non_column_values$Protocol <- determine_protocol(stimdata)







# In our analyses for MP task, the "target" image corresponds to the
# "familiar" image. In the data, however, this is not the case, so we need to
# flip the target-image location label for mispronounced and nonsense word
# trials. In cheatsheet form:
# ```
#               Before correction:            After correction:
#   StimType    Target        Distractor      Target        Distractor
#   'real'      familiar      unfamiliar      familiar      unfamiliar
#   'MP'        unfamiliar    familiar        familiar      unfamiliar
#   'nonsense'  unfamiliar    familiar        familiar      unfamiliar
# ```

targets <- stimdata$Target
flip_me <- is.element(stimdata$StimType, c("MP", "nonsense"))
targets <- ifelse(flip_me, flip_image(targets), targets)
stimdata$TargetImage <- targets
stimdata$DistractorImage <- flip_image(stimdata$TargetImage)



glimpse(stimdata)
stopifnot(all(stimdata$DistractorImage != stimdata$TargetImage))

stimdata$Target <-
  teleport_by_column(stimdata, "TargetImage") %>%
  # Convert image-filename to word-label
  str_replace("\\d$", "")

lefts <- which(stimdata$TargetImage == "ImageL")
right <- which(stimdata$TargetImage == "ImageR")
stimdata[lefts, "Target"]
stimdata[right, "Target"]

# Make number-ful columns into numeric values
stimdata <- stimdata %>% lapply(try_numeric) %>% as_data_frame

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

str_filter <- function(xs, pattern) {
  xs[str_detect(xs, pattern)]
}




durs <- str_filter(names(stimdata), "Dur$")
onsets <- str_replace(durs, "Dur", "Onset")

if ()

stimdata$AudioDur

is.element(onsets, names(stimdata))


stimdata$AttentionOnset
ref_stimdata$TargetEnd
ref_stimdata$AttentionOnset


setdiff(names(ref_stimdata), names(stimdata))
# [1] "Protocol"         "xxxx"       "xxxx" "TargetDur"
# [5] "Dialect"          "AttentionEnd"     "AttentionOnset"   "FixationDur"
# [9] "CarrierEnd"       "TargetEnd"

stimdata %>%
  mutate(
    AttentionEnd = AttentionOnset + AttentionDur,
    )

stimdata$FixationDur












ParseFilename()







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

