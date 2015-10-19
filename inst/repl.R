# Experiment to see if I can make an easier route into the package (one that
# does not assume Tobii/Eprime sources)

# Load and parse a Tobii Gazedata file
library("lookr")
all_looks <- Gazedata("tests/testthat/data/Coartic_WFFArea_1a/001P00XS1/Coartic_Block1_001P00XS1.gazedata")

# Load and parse corresponding Eprime experiment log
library("rprime")
exp_log <- read_eprime("tests/testthat/data/Coartic_WFFArea_1a/001P00XS1/Coartic_Block1_001P00XS1.txt")
stim_df <- to_data_frame(keep_levels(FrameList(exp_log), level_numbers = 3))

# convert "1" to 1
stim_df <- readr::type_convert(stim_df)
str(stim_df)

# Create one Trial
looks <- subset(all_looks, TrialNo == 1)
stim <- subset(stim_df, Sample == 1)
str(stim_df)

#' Create a Trial object
Trial <- function(looks, stim) {

  stopifnot(is.data.frame(looks))
  stopifnot(is.list(stim))

  # Attach the stimdata attributes
  attributes(looks) <- c(attributes(looks), stim)

  # Check for expected attributes. TODO add more
  checks <- list(
    Subject = "a participant identifier",
    Basename = "name of source file of the trial info",
    TrialNo = "trial number"
  )

  # Check for expected attributes. TODO add more
  looks_checks <- list(
    Basename = "name of source file of the trial info",
    Time = "time of the gaze sample (milliseconds)"
  )

  for (check_name in names(checks)) {
    if (is.null(looks %@% check_name)) {
      this_warn <- sprintf("Expected trial attribute: stim$%s (%s)",
                           check_name, checks[[check_name]])
      warning(this_warn)
    }
  }


  for (check_name in names(looks_checks)) {
    if (is.null(looks[[check_name]])) {
      this_warn <- sprintf("Expected gaze column: looks$%s (%s)",
                           check_name, checks[[check_name]])
      warning(this_warn)
    }
  }


  # TODO force Basename to match in both stim and looks

  as.Trial(looks)
}

# Expect a bunch of warnings
t1 <- Trial(looks, stim1)


# Fix warnings by creating the expected columns
library("stringr")

# Rename two columns to create Basename and TrialNo
rename_scheme <- c(Eprime.Basename = "Basename", Sample = "TrialNo")
names(stim_df) <- str_replace_all(names(stim_df), rename_scheme)

# Derive Subject from Basename
stim_df$Subject <- str_extract(stim_df$Basename, "\\d{3}.*")

# Try again
stim1 <- subset(stim_df, TrialNo == 1)
t1 <- Trial(looks, stim1)


# Create several Trials, from two data-frames
looks <- all_looks
on <- "TrialNo"

# Vectorized/Listified form of the above
TrialList <- function(looks, stim, on = c("Basename", "TrialNo")) {
  # Split by Basename x TrialNo
  look_chunks <- split(looks, looks[on])
  stim_chunks <- split(stim, stim[on])
  # Keep only items in both
  trial_names <- intersect(names(look_chunks), names(stim_chunks))
  # todo warning about excluded ones

  # Combine looks and stimulus data for each trial name
  unite_by_name <- function(n) Trial(look_chunks[[n]], stim_chunks[[n]])
  trials <- Map(unite_by_name, trial_names)
  as.TrialList(unname(trials))
}

on <- c("Basename", "TrialNo")
trials <- TrialList(looks, stim_df, c("Basename", "TrialNo"))
trials %@% "Subject"
trials %@% "TrialNo"
trials %@% "Target"
trials

library("ggplot2")
qplot(x = Time, y = XMean, data = trials[[1]])
trials[[1]]

# Align by Target Onset
trials <- AdjustTimes(trials, "Target.OnsetTime")
qplot(x = Time, y = XMean, data = trials[[1]])

events <- list(list("Target.OnsetTime", "CarrierStim.OnsetTime", "Fixation.OnsetTime", "Image2sec.OnsetTime"))
trials %@% "Events" <- events
trials[[1]]

# Trim trial boundaries
trials <- TimeSlice(trials, -500, 2000)
qplot(x = Time, y = XMean, data = trials[[1]])

AddAOIData(trials)
AddAOIData.Trial
GetImageAOI
GetFramesWithGazeInAOI
attrs <- c("TrialNo", "WordGroup", "Events")
gather_attributes(trials, )


gather_attributes
x <- trials[[1]]

# function(x, attrs, omit_na = FALSE) {
#   # Set names of unnamed attributes
#   if (is.null(names(attrs))) names(attrs) <- attrs
#   names(attrs) <- ifelse(names(attrs) == "", attrs, names(attrs))
#
#   # Get each attribute
#   get_this_attr <- function(this_attr) x %try@% this_attr
#   attr_list <- lapply(attrs, get_this_attr)
#
#   Filter(function(x) length(x) == 1, attr_list)
#   attr_list <- lapply(attrs, length)
#
#   # List names are inherited from the names of the attrs
#   stopifnot(names(attr_list) == names(attrs))
#   attr_table <- as.data.frame(attr_list, stringsAsFactors = FALSE)
#
#   if (omit_na) {
#     # Which columns have all NA values
#     all_nas <- unlist(colwise(is_all_na)(attr_table))
#     to_drop <- names(attr_table)[all_nas]
#
#     # Null them
#     attr_table[, to_drop] <- list(NULL)
#   }
#
#   attr_table
# }
#
#
#
