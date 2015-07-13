#' Compute the average duration of tracked gazes
#'
#' To quantify "flicker" in eye-tracking data, Wass, Smith and Johnson (2013)
#' found the average duration of tracked segments of eye-tracking data. This
#' function performs this calculation, updating trials with a new attribute
#' \code{AvgGazeContact}. This value measures the average duration (ms) of each
#' segment of uninterrupted eye-tracking data in a trial.
#'
#' @param x a Trial with a \code{GazeByImageAOI} column or a TrialList where
#'   each Trial has a \code{GazeByImageAOI} column
#' @return the Trial object(s) updated to include the attribute
#'   \code{AvgGazeContact}
#' @export
ComputeGazeContact <- function(x) UseMethod("ComputeGazeContact")

#' @export
ComputeGazeContact.TrialList <- function(x) trial_lapply(x, ComputeGazeContact)

#' @export
ComputeGazeContact.Trial <- function(x) {
  trial <- x
  stopifnot("GazeByImageAOI" %in% names(trial))
  looks <- rle(trial$GazeByImageAOI)
  uninterrupted <- looks$lengths[!is.na(looks$values)]
  contact <- round(mean(uninterrupted) * lwl_constants$ms_per_frame)
  trial %@% "AvgGazeContact" <- contact
  trial
}






#' Calculate image bias within a window of time
#'
#' Bias answers the question, "Which image was this subject viewing during a
#' certain window of time?" For this calculation, we count the number of frames
#' spent on each AOI and multiply the frame-counts by the duration of the
#' window. Over a 100ms (6 frame) window, 3 frames on one AOI would receive a
#' weight of 300. Ties are broken by choosing the image with the earliest
#' fixation. In that same 6 frame window, if 3 frames are spent each on 2 AOIs,
#' the weights would be tied, so the AOI that was fixated on first would be
#' chosen.
#'
#' @param x a Trial with a \code{GazeByImageAOI} column or a TrialList where
#'   each Trial has a \code{GazeByImageAOI} column
#' @param window a twq-element vector specifying the window of time within which
#'   to calculate the image bias.
#' @return the Trial object(s) updated to include two new attributes:
#'   \code{Bias}, the name of the AOI with the highest bias or \code{NA} if
#'   there are no looks to the AOIs during the time window; and
#'   \code{BiasSummary}, a data-frame detailing the bias calculation.
#' @export
CalculateBias <- function(x, window) UseMethod("CalculateBias")

#' @export
CalculateBias.TrialList <- function(x, window) {
  trial_lapply(x, CalculateBias, window)
}

#' @export
CalculateBias.Trial <- function(x, window) {
  trial <- x
  # Make a copy of the trial for modification
  subtrial <- ExtractWindow(trial, window)

  # Calculate the bias by weighting each frame using the longest possible
  # fixation duration in the window and break ties by using the earliest image.
  subtrial$WindowDur <- nrow(subtrial) * lwl_constants$ms_per_frame
  subtrial$LastPossible <- max(subtrial$Time)

  bias_summary <- ddply(subtrial, "GazeByImageAOI", summarize,
                        Frames = length(Time),
                        Weight = Frames * unique(WindowDur),
                        # Break ties by choosing the earlier viewed AOI
                        FirstLook = min(Time),
                        Earliness = unique(LastPossible) - FirstLook,
                        Bias = Weight + Earliness
  )

  # We only care about looks to real AOIs
  valid_aois <- !is.element(bias_summary$GazeByImageAOI, c(NA, "tracked"))
  bias_summary <- bias_summary[valid_aois, ]

  # Use NA as a place-holder attribute if there are no looks to the AOIs
  if (nrow(bias_summary) == 0) {
    winner <- NA_character_
  } else {
    winner <- bias_summary[which.max(bias_summary$Bias), "GazeByImageAOI"]
  }

  # Update the trial
  trial %@% "Bias" <- winner
  trial %@% "BiasSummary" <- bias_summary
  trial
}

ExtractWindow <- function(trial, window) {
  window <- sort(window)
  trial <- trial[window[1] <= trial$Time & trial$Time <= window[2], ]
  trial
}


#' Compute dwell times
#'
#' A "dwell" is an uninterrupted gaze. This function calculates the longest
#' dwell time per image and the total time dwelling in each trial.
#'
#' @inheritParams ComputeGazeContact
#' @return the Trial object(s) updated to include a \code{DwellSummary}
#'   attribute which contains a dataframe describing the maximum and total dwell
#'   times for each image location and stimulus.
#' @export
GetDwellTimes <- function(x) UseMethod("GetDwellTimes")

#' @export
GetDwellTimes.TrialList <- function(x) trial_lapply(x, GetDwellTimes)

#' @export
GetDwellTimes.Trial <- function(x) {
  trial <- x
  stopifnot("GazeByImageAOI" %in% names(trial), "GazeByAOI" %in% names(trial))
  # rle doesn't compute streaks for NAs so use fake NA values
  use_soft_nas <- function(xs) ifelse(is.na(xs), "NA", xs)
  use_hard_nas <- function(xs) ifelse(xs == "NA", NA, xs)
  aoi_run <- rle(use_soft_nas(trial$GazeByAOI))
  stim_run <-  rle(use_soft_nas(trial$GazeByImageAOI))

  # Calculate dwell times
  streaks <- data.frame(
    Location = use_hard_nas(aoi_run$values),
    Stimulus = use_hard_nas(stim_run$values),
    DwellTime = stim_run$lengths * lwl_constants$ms_per_frame,
    stringsAsFactors = FALSE)

  # Determine max and proportion
  dwell_summary <- ddply(streaks, .(Location, Stimulus), summarise,
    LongestDwellTime = max(DwellTime),
    TotalDwellTime = sum(DwellTime))
  dwell_summary <- mutate(dwell_summary,
    DwellProp = TotalDwellTime / sum(TotalDwellTime))
  trial %@% "DwellSummary" <- dwell_summary
  trial
}




#' Calculate the amount of missing data in a list of Trials
#'
#' @inheritParams ComputeGazeContact
#' @param column A character string that names one of the columns of Gazedata in
#'   the trial object. Default is `"GazeByImageAOI"`.
#' @return the Trial object(s) updated to include attributes about the amount of
#'   missing data
#' @export
CalculateMistrackings <- function(x, column = "GazeByImageAOI") {
  UseMethod("CalculateMistrackings")
}

#' @export
CalculateMistrackings.TrialList <- function(x, column = "GazeByImageAOI") {
  trial_lapply(x, CalculateMistrackings, column)
}

#' @export
CalculateMistrackings.Trial <- function(x, column = "GazeByImageAOI") {
  trial <- x
  # Eval/substitute so that the value of `column` is printed in the error msg
  eval(substitute(stopifnot(n %in% names(trial)), list(n = column)))
  mistrackings <- sum(is.na(trial[[column]]))
  num_frames <- length(trial[[column]])
  trial %@% "MistrackedFrames" <- mistrackings
  trial %@% "NumberOfFrames" <- num_frames
  trial %@% "PropNA" <- mistrackings / num_frames
  trial %@% "PropTracked" <- 1 - (mistrackings / num_frames)
  trial
}





#' Interpolate missing gaze data
#'
#' @inheritParams ComputeGazeContact
#' @param window the time window (in ms.) to interpolate over
#' @return the Trial object(s) with missing data interpolated
#' @export
InterpolateMissingFrames <- function(x,  window = lwl_opts$get("interpolation_window")) {
  UseMethod("InterpolateMissingFrames")
}

#' @export
InterpolateMissingFrames.TrialList <- function(x, window = lwl_opts$get("interpolation_window")) {
  trial_lapply(x, InterpolateMissingFrames, window)
}

#' @export
InterpolateMissingFrames.Trial <- function(x, window = lwl_opts$get("interpolation_window")) {
  trial <- x
  trial %@% "InterpolatedPoints" <- 0
  trial %@% "CorrectedFrames" <- numeric(0)
  trial %@% "CorrectedTimes" <- numeric(0)

  # Convert window duration (ms) into the number of frames, rounded down.
  frames_in_window <- floor(window / lwl_constants$ms_per_frame)

  # Extract the gazes from the trial. Record how many missing frames there are.
  gazes <- trial$GazeByImageAOI
  missing <- sum(is.na(gazes))

  # Grab all the non-NA gaze frames.
  tracked <- which(!is.na(gazes))

  # The lag in frame numbers of non-NA gazes tells us how many NA frames were
  # skipped when we extracted all the non-NA gazes. Include the 0 at front
  # because diff(1:n) returns n-1 values
  differences <- diff(c(0, tracked))

  ## Find starts and ends of each NA gap

  # Locations from `which` are not accurate because they don't take into account
  # earlier missing frames. Use the cumulative sum of missing frames to correct
  # these start locations.
  gap_start <- which(1 < differences)
  gap_size <- differences[gap_start] - 1
  total_gap_sizes <- cumsum(gap_size)
  # First gap doesn't need to be offset
  start_offsets <- c(0, total_gap_sizes[-length(total_gap_sizes)])
  gap_start <- gap_start + start_offsets - 1
  gap_end <- gap_start + gap_size + 1

  # Enforce valid windows! Margins need to be non-NA and next to an NA value
  stopifnot(is.na(gazes[c(gap_start + 1, gap_end - 1)]),
            !is.na(gazes[c(gap_start, gap_end)]))

  # Make a set of Gap objects from these start/end/size descriptions
  gaps <- Map(Gap, gap_start, gap_end, gap_size)

  # Only fill gaps no bigger than the interpolation window, gaps that don't
  # involve first frame and gaps with the gaze location on both sides of window
  has_legal_length <- function(gap) gap$na_size <= frames_in_window
  is_not_first_frame <- function(gap) gap$start != 0
  is_fillable <- function(gap) gazes[gap$start] == gazes[gap$end]
  has_legal_aois <- function(gap) gazes[gap$start] != "tracked"

  gaps <- Filter(has_legal_length, gaps)
  gaps <- Filter(is_not_first_frame, gaps)
  gaps <- Filter(is_fillable, gaps)
  gaps <- Filter(has_legal_aois, gaps)

  # Fill each gap
  for (gap in gaps) {
    trial <- fill_gap(trial, gap)
  }

  # Update the AOI Data using the interpolated values
  AddAOIData(trial)
}

# Interpolate a single gap
fill_gap <- function(trial, gap) {
  # Compute interpolation values
  columns <- c("XLeft", "XRight", "XMean", "YLeft", "YRight", "YMean",
               "XLeftToTarget", "XRightToTarget", "XMeanToTarget",
               "YLeftToTarget", "YRightToTarget", "YMeanToTarget")
  means <- sapply(trial[gap$seq, columns], mean, na.rm = TRUE)

  # Interpolate each column
  for (col in columns) {
    trial[gap$na_seq, col] <- means[col]
  }

  trial %@% "CorrectedFrames" <- c(trial %@% "CorrectedFrames", gap$na_seq)
  trial %@% "CorrectedTimes" <- trial$Time[trial %@% "CorrectedFrames"]
  trial %@% "InterpolatedPoints" <- trial %@% "InterpolatedPoints" + gap$na_size
  trial
}

# Simple container for the information we care about when interpolating a gap
Gap <- function(start, end, na_size) {
  structure(list(
    start = start, end = end, na_size = na_size,
    seq = seq(start, end), na_seq = seq(start + 1, end - 1)),
    class = c("Gap", "list"))
}

