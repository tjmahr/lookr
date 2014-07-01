#' Compute the average duration of tracked gazes
#'
#' To quantify "flicker" in eye-tracking data, Wass, Smith and Johnson (2013)
#' found the average duration of tracked segments of eye-tracking data. This
#' function performs this calculation, updating trials with a new attribute
#' \code{AvgGazeContact}. This value measures the average duration (ms) of each
#' segment of uninterrupted eye-tracking data in a trial.
#'
#' @param trial a Trial object with a \code{GazeByImageAOI} column
#' @param trials a list of Trial objects, each with a \code{GazeByImageAOI}
#'   column
#' @return the Trial or list of Trials updated with a new attribute
#'   \code{AvgGazeContact}
#' @export
ComputeGazeContact <- function(...) UseMethod("ComputeGazeContact")

#' @export
ComputeGazeContact.list <- function(trials) {
  trial_lapply(trials, ComputeGazeContact)
}

#' @export
ComputeGazeContact.Trial <- function(trial) {
  looks <- rle(trial$GazeByImageAOI)
  uninterrupted <- looks$lengths[!is.na(looks$values)]
  contact <- round(mean(uninterrupted) * lwl_constants$ms_per_frame)
  trial %@% "AvgGazeContact" <- contact
  trial
}



#' Calculate the amount of missing data in a list of Trials
#'
#' @inheritParams ComputeGazeContact
#' @param column A character string that names one of the columns of Gazedata in
#'   the trial object. Default is `"GazeByImageAOI"`.
#' @return A list of Trial objects that has been updated to include attributes
#'   containing percentage of mistracked data.
#' @export
CalculateMistrackings <- function(...) UseMethod("CalculateMistrackings")

#' @export
CalculateMistrackings.list <- function(trials, column = "GazeByImageAOI") {
  trial_lapply(trials, CalculateMistrackings, column)
}

#' @export
CalculateMistrackings.Trial <- function(trial, column = "GazeByImageAOI") {
  mistrackings <- sum(is.na(trial[[column]]))
  num_frames <- length(trial[[column]])
  trial %@% "MistrackedFrames" <- mistrackings
  trial %@% "NumberOfFrames" <- num_frames
  trial %@% "PercentNA" <- mistrackings / num_frames
  trial
}


















#' Interpolate missing gaze data
#'
#' @inheritParams ComputeGazeContact
#' @param window the time window (in ms.) to interpolate over
#' @return the list of Trials with missing data interpolated
#' @export
InterpolateMissingFrames <- function(...) UseMethod('InterpolateMissingFrames')

#' @export
InterpolateMissingFrames.list <- function(trials, window = lwl_opts$get("interpolation_window")) {
  trial_lapply(trials, InterpolateMissingFrames, window)
}

#' @export
InterpolateMissingFrames.Trial <- function(trial, window = lwl_opts$get("interpolation_window")) {
  message(trial %@% "TrialNo")
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

  gaps <- Filter(has_legal_length, gaps)
  gaps <- Filter(is_not_first_frame, gaps)
  gaps <- Filter(is_fillable, gaps)

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

