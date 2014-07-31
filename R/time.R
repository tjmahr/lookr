# This script collects timing-related functions used in reducing and analyzing
# data from Looking While Listening eyetracking experiments.


#' Adjust trial times using a trial attribute
#'
#' @param x a Trial or a TrialList object
#' @param event the name of the Trial attribute to be used as time 0. The
#'   default is \code{"TargetOnset"}.
#' @return the Time column of the Trial(s) is updated so that time 0 occurs at
#'   the time given by attribute. For example, if \code{attr(trial,
#'   "TargetOnset")} is 1000 (ms), then the frame in the Time column that is
#'   closest to 1000 gets the new value 0 and all other frames and time-related
#'   attributes are updated relative to this value.
#' @export
AdjustTimes <- function(x, event = lwl_opts$get("alignment_event")) {
  UseMethod("AdjustTimes")
}

#' @export
AdjustTimes.TrialList <- function(x, event = lwl_opts$get("alignment_event")) {
  trial_lapply(x, AdjustTimes, event)
}

#' @export
AdjustTimes.Trial <- function(x, event = lwl_opts$get("alignment_event")) {
  trial <- x
  stopifnot(event %in% names(attributes(trial)))
  zero_frame <- FindClosestFrame(trial, trial %@% event)
  trial <- AssignNewTimes(trial, zero_frame = zero_frame)
  trial <- AdjustTimingMarks(trial, event)
  trial
}


#' Align trial objects in a list
#'
#' @description
#' This function normalizes trials by length and by timing such that:
#'
#' \itemize{
#'   \item all trials have the same number of frames
#'   \item the event specified as time 0 occurs in the same frame in each trial
#'   \item frames at the same index have the same time (in ms.) across trials
#' }
#'
#' Because trials may vary in length, empty frames may be added to the beginning
#' or end of a trial to normalize the frame-length of trials. These are filled
#' with NA gaze values. Trials also have updated timing attributes.
#'
#' @param trials
#' @param alignment_event default is \code{"TargetOnset"}
#' @return the inputted set of trials with normalized lengths (in frame
#'   numbers), normalized \code{$Time} values (in ms.) and updated timing
#'   attributes.
#' @export
AlignTrials <- function(trials, alignment_event = lwl_opts$get("alignment_event")) {
  # Find the frame of each Trial that occurred closest to the alignment event.
  alignment_times <- trials %@% alignment_event
  zero_frames <- mapply(FindClosestFrame, trials, alignment_times)

  # Find the number of frames each Trial must have before the zero-mark
  # in order for it to be possible to align all the trials.
  before_zero <- zero_frames - 1
  before_zero <- max(before_zero) - before_zero

  # Add frames at the beginning of each Trial.
  aligned_at_start <- Map(.MakeFramePadder('start'), trials, before_zero)

  # Find the number of frames each Trial must have after the zero-mark in order
  # for it to be possible to align all the trials.
  after_zero <- mapply(.ComputeFramesAfterZero, trials, zero_frames)
  after_zero <- max(after_zero) - after_zero

  # Add frames at the end of each Trial.
  aligned <- Map(.MakeFramePadder('end'), aligned_at_start, after_zero)

  # Correct the zero-mark frame, now that the Trials have been aligned.
  zero_frames <- zero_frames + before_zero

  # Align the `$Time` of each Trial so that the alignment frame occurs at 0.
  aligned <- Map(AssignNewTimes, aligned, zero_frames)

  # Adjust all the timing attributes of the Trials.
  CurryAdjustTimingMarks  <- function(trial) AdjustTimingMarks(trial, alignment_event)
  aligned <- Map(CurryAdjustTimingMarks, aligned)

  # Reset the class of aligned to the original classes of trials.
  class(aligned) <- class(trials)
  aligned
}



#' Find frame of a trial that is closest to a given time
#'
#' If two frames are temporally equidistant from the given time, choose the
#' earlier frame.
#'
#' @param trial a \code{Trial} object
#' @param time a \code{numeric} value (in ms.)
#' @return The number of the frame of the trial whose time is closest to the
#'   given time.
FindClosestFrame <- function(trial, time) {
  time_distance <- abs(trial$Time - time)
  closest_rows <- which(time_distance == min(time_distance))
  # Break ties: Select the (first) row that contains the smallest Time value
  closest_rows[which.min(trial$Time[closest_rows])]
}




#' Make a function for padding a trial with empty frames
#'
#' @param location the margin onto which we add the empty frames, either
#'   \code{"start"} or \code{"end"}.
#' @param trial in the returned function, the \code{Trial} onto which to add the
#'   empty frames.
#' @param add_frames in the returned function, the number of empty frames to add
#'   onto \code{trial}
#' @return a function for adding frames to the beginning or end of a set of
#'   trials.
#' @examples
#' # Add frames at the beginning of each Trial in a list of Trial.
#' aligned_at_start <- Map(.MakeFramePadder('start'), trials, before_zero)
.MakeFramePadder <- function(location) {
  function(trial, add_frames) {
    # Initialize expanded dataframe
    new_frames <- matrix(data = NA, nrow = add_frames, ncol = dim(trial)[2])
    new_frames <- data.frame(new_frames)
    names(new_frames) <- names(trial)
    # Pad frames onto the specified margin
    if (location == 'start') {
      with_new_frames <- rbind(new_frames, trial)
    } else if (location == 'end') {
      with_new_frames <- rbind(trial, new_frames)
    }
    # Update attributes
    new_attributes <- attributes(trial)
    new_attributes$row.names <- 1:(dim(with_new_frames)[1])
    attributes(with_new_frames) <- new_attributes
    with_new_frames
  }
}




#' Find number of frames in a trial that occur after a reference frame
#'
#' @param trial
#' @param align_frame an \code{integer} index of the reference frame to begin
#'   counting after.
#' @return the number of frames that occur after the reference frame
.ComputeFramesAfterZero <- function(trial, align_frame) {
  length(trial$Time) - align_frame
  # dim(trial)[1] - align_frame
}


#' Assign new times to a trial once it has been aligned at a reference frame
#'
#' @param trial a Trial object
#' @param the index of the reference frame that will mark time = 0 in the
#'   aligned trial
#' @return the inputted trial object with updated values in its \code{Time}
#'   column
AssignNewTimes <- function(trial, zero_frame) {
  centered_frames <- seq_along(along.with = trial$Time) - zero_frame
  trial$Time <- centered_frames * lwl_constants$ms_per_frame
  trial
}




#' Adjust the event-timing attributes of a trial
#'
#' @keywords internal
#' @param trial
#' @param alignment_event
#' @return the inputted trial object with updated timing attributes
AdjustTimingMarks <- function(trial, alignment_event) {
  adjust_by <- trial %@% alignment_event
  events <- c('ImageOnset', 'CarrierOnset', 'CarrierEnd', 'TargetOnset',
              'TargetEnd', 'AttentionOnset', 'AttentionEnd', 'FixationOnset')
  for (event in events) trial %@% event <- (trial %@% event - adjust_by)

  # Add an attribute which tracks where the trial was aligned.
  timing_attributes <- attributes(trial)[names(attributes(trial)) %in% events]
  alignment_names <- names(which(timing_attributes == 0))
  trial %@% 'AlignedBy' <- alignment_names
  trial
}



#' Align the zero-time frames and normalize lengths of a list of trials
#'
#' @description
#' Because of the possible \code{TargetOnsetDelay} in Wait-For-Fixation trials,
#' a list of a trials that have been time-aligned and time-sliced may have
#' differing number of frames. This function truncates all trials so they have
#' the same number of frames and so that the frame at time = 0 occurs in the
#' same place across all the list of trials.
#'
#' Two steps are involved in the alignment process: (1) Removing frames from the
#' beginning of the trials so that zero frame occurs in the same frame across
#' all the trials, then (2) removing frames from the end of the trials so that
#' all they all have the same number of frames.
#'
#' @param trials a \code{list} of \code{Trial} objects
#' @return a truncated and zero-frame aligned \code{list} of \code{Trials}
AlignZeroFrames <- function(trials) {
  # Preserve class names
  trial_classes <- class(trials)

  # Align the zero frames.
  onset_frames <- GetFrameAtTime(trials, 0)
  earliest_onset <- min(onset_frames)
  trials <- Map(.AlignOnset, trials, earliest_onset)

  # Truncate trials so they are the same length.
  shortest_length <- min(GetTrialLengths(trials))
  trials <- Map(.TruncateTrial, trials, shortest_length)

  # Preserve class of "Trials" if necessary.
  class(trials) <- trial_classes
  return(trials)
}


#' Normalize the number of frames before time zero in a Trial
#'
#' This function supports the \code{AlignZeroFrame} function. It lops off frames
#' from the beginning of a \code{Trial} if its \code{TargetOnset} (time = zero)
#' frame occurs later than the earliest \code{TargetOnset} frame index.
#'
#' A warning is printed when the truncation occurs.
#'
#' @param trial a \code{Trial} object to be truncated.
#' @param earliest_onset the index of the frame where the zero-time frame should
#'   go.
#' @return a \code{Trial} object with a time = 0 occuring at the frame number
#'   specified by \code{earliest_onset}.
.AlignOnset <- function(trial, earliest_onset) {
  # Determine if the Trial needs to be truncated.
  trial_onset <- which(trial$Time == 0)
  difference <- trial_onset - earliest_onset

  # Truncate the Trial.
  if (0 < difference) {
    # Specify boundaries of truncation.
    first_frame <- difference + 1
    length_trial <- length(trial$Time)

    # Truncate the trial and update its NumberOfFrames attribute.
    trial <- trial[first_frame:length_trial, ]
    trial %@% "NumberOfFrames" <- length_trial - difference

    # Alert the user to the truncation.
    warn <- paste("trial", trial %@% "TrialNo", "for subject",
                  trial %@% "Subject", "truncated by", difference,
                  "frame(s) when TargetOnset frames were aligned.")
    warning(warn, call. = FALSE, immediate. = TRUE)
  }
  return(trial)
}


#' Normalize the number of frames in a Trial
#'
#' This function supports the \code{AlignZeroFrame} function. It lops off frames
#' from the end of a \code{Trial} if its longer than the shortest trial.
#'
#' A warning is printed when the truncation occurs.
#'
#' @param trial a \code{Trial} object to be truncated.
#' @param shortest_length the number of frames that the \code{Trial} should be
#'   in length.
#' @return the truncated \code{Trial}
.TruncateTrial <- function(trial, shortest_length) {
  # Determine if the Trial needs to be truncated.
  length_trial <- length(trial$Time)
  difference <- length_trial - shortest_length

  # Truncate the Trial
  if (0 < difference) {
    # Truncate the trial and update its length attribute.
    trial <- trial[1:shortest_length, ]
    trial %@% "NumberOfFrames" <- shortest_length

    # Alert the user to the truncation
    warn <- paste("trial", trial %@% "TrialNo", "for", "subject",
                  trial %@% "Subject", "truncated by", difference,
                  "frame(s) when trials were made same length.")
    warning(warn, call. = FALSE, immediate. = TRUE)
  }
  return(trial)
}




#' Extract a subset of trial data, congruent to bin size
#'
#' We only really care about part of each trial, so we should extract that time
#' interval from each trial and not worry about the rest of the trial. Our
#' log-odds function however analyzes AOI data 3 bins at a time (or in 49.9638
#' ms chunks), so we should make sure that our interval can be broken into bins
#' (i.e., broken into 49.9638 ms chunks).
#'
#' This function extends the time interval for extraction to the nearest bin.
#' The result is that the number of time frames in the extracted interval is
#' evenly divisible by the bin size.
#'
#' @param trials a list of trials
#' @param start_time the starting time (ms) of the interval
#' @param end_time the end time (ms) of the interval
#' @param bin_size the number of time frames per bin (default is 3)
#' @return a list of \code{trial} objects with the desired interval extracted
#'   and extended so that the number of time frames is evenly divisisble by the
#'   bin size
BinWiseTimeSlice <- function(trials, start_time, end_time, bin_size = 3) {
  difference <- end_time - start_time
  bin_duration <- bin_size * lwl_constants$ms_per_frame
  bin_count <- ceiling(difference / bin_duration)
  slice_duration <- bin_count * bin_size * lwl_constants$ms_per_frame
  # We subtract a frame from `real_end_time` to ensure that TimeSlice returns an
  # interval of `bin_count * bin_size` frames in length.
  real_end_time <- slice_duration + start_time - lwl_constants$ms_per_frame
  sliced <- TimeSlice(trials, start_time, real_end_time)
  return(sliced)
}






#' Get the lengths (in frames) of a list of trials
#'
#' @keywords internal
#' @param trials a list of \code{Trial} objects.
#' @return a numeric vector with the number of frames in each \code{Trial}.
GetTrialLengths <- function(trials) sapply(trials, nrow)


#' Get the frame number for a certain Time value in a list of Trials
#'
#' @keywords internal
#' @param trials a list of \code{Trial} objects
#' @param time_point a numeric value indicating the time to find in the Trial
#' @return a numeric vector listing the frames at which the desired time occurs
#'   in each \code{Trial}
GetFrameAtTime <- function(trials, time_point = 0) {
  # Helper function that returns the frame where a time occurs in a vector.
  which_time <- function(times, point = time_point) which(times == time_point)
  # Find the time-point in each of the Time columns
  times <- Map(getElement, trials, "Time")
  sapply(times, which_time)
}



















#### Timeslicing --------------------------------------------------------------


#' Extract an interval of time in a Trial
#'
#' @param x a Trial or a TrialList object
#' @param from the time at which to start slicing. This value can be a
#'   \code{character} string naming a valid timing attribute of the Trial (e.g.,
#'   \code{"TargetOnset"}), a numeric value specifying a particular time, or
#'   \code{NULL} in which case the function slices from the first frame of time.
#' @param to time to which to finish slicing. The parameter may be of the same
#'   classes as described above for \code{from}, but when \code{NULL} is passed
#'   a value, the final time frame is used for slicing.
#' @return the Time column of the Trial(s) is updated so that minimum time is
#'   within one frame of \code{from} and the maximum time is within one frame of
#'   {to}. The updated Trial(s) also has the added attribute
#'   \code{"NumberOfFrames"}.
#' @export
TimeSlice <- function(x, from = lwl_opts$get("timeslice_start"),
                      to = lwl_opts$get("timeslice_end")) {
  UseMethod('TimeSlice')
}

#' @export
TimeSlice.TrialList <- function(x, from = lwl_opts$get("timeslice_start"),
                                to = lwl_opts$get("timeslice_end")) {
  trial_lapply(x, TimeSlice, from, to)
}

#' @export
TimeSlice.Trial <- function(x, from = lwl_opts$get("timeslice_start"),
                            to = lwl_opts$get("timeslice_end")) {
  trial <- x
  # Resolve what times are meant by `from` and `to`.
  from <- switch(class(from), `character` = trial %@% from, `numeric` = from,
                 min(trial$Time))
  to <- switch(class(to), `character` = trial %@% to, `numeric` = to,
               max(trial$Time))
  stopifnot(min(trial$Time) <= from, to <= max(trial$Time), from < to)

  # Convert the start and end times into the corresponding frame numbers.
  start_index <- max(which(trial$Time <= from))
  end_index <- max(which(trial$Time <= to))

  # Slice, then attach the number of frames as an attribute.
  trial <- trial[seq(start_index, end_index), ]
  trial %@% "NumberOfFrames" <- nrow(trial)
  trial
}
