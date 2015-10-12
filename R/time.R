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
                 `integer` = from, min(trial$Time))
  to <- switch(class(to), `character` = trial %@% to, `numeric` = to,
               `integer` = to, max(trial$Time))
  stopifnot(from < to)

  # Warn if TimeSlice window bigger than available data
  if (from < min(trial$Time) | max(trial$Time) < to) {
    l1 <- sprintf("Trial %s in %s is smaller than TimeSlice window",
                  trial %@% "TrialNo", trial %@% "Basename")
    l2 <- sprintf(".. TimeSlice window: %s:%s. Times in trial: %s:%s",
                  from, to, ceiling(min(trial$Time)), floor(max(trial$Time)))

    # Update window
    from <- ceiling(max(from, min(trial$Time)))
    to <- floor(min(to, max(trial$Time)))
    l3 <- sprintf(".. Using new window %s:%s", from, to)

    warning(paste(l1, l2, l3, sep = "\n"))
  }

  # Convert the start and end times into the corresponding frame numbers.
  start_index <- max(which(trial$Time <= from))
  end_index <- min(which(to <= trial$Time))

  # Slice, then attach the number of frames as an attribute.
  trial <- trial[seq(start_index, end_index), ]
  trial %@% "NumberOfFrames" <- nrow(trial)
  trial
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


#' Assign new times to a trial once it has been aligned at a reference frame
#'
#' @param trial a Trial object
#' @param zero_frame the index of the reference frame that will mark time = 0 in the
#'   aligned trial
#' @param frame_rate the eye-tracker sampling rate. Defaults to
#'   \code{lwl_constants$ms_per_frame} which is 16.6546ms.
#' @return the inputted trial object with updated values in its \code{Time}
#'   column and an added \code{FrameRate} attribute
AssignNewTimes <- function(trial, zero_frame, frame_rate = lwl_constants$ms_per_frame) {
  centered_frames <- seq_along(along.with = trial$Time) - zero_frame
  trial$Time <- centered_frames * frame_rate
  trial %@% "FrameRate" <- frame_rate
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
