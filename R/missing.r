



#' Compute the average duration of tracked gazes
#' 
#' To quantify "flicker" in eye-tracking data, Wass, Smith and Johnson (2013) 
#' found the average duration of tracked segments of eye-tracking data. This
#' function performs this calculation, updating trials with a new attribute
#' `AvgGazeContact`. This value measures the average duration (ms) of each
#' segment of uninterrupted eye-tracking data in a trial.
#' 
#' @param trial a Trial object with a `GazeByImageAOI` column
#' @param trials a list of Trial objects, each with a `GazeByImageAOI` column
#' @return the Trial or list of Trials updated with a new attribute 
#'   `AvgGazeContact`
#' @export
ComputeGazeContact <- function(...) UseMethod("ComputeGazeContact")

#' @export
ComputeGazeContact.list <- function(trials) {
  classes <- class(trials)
  trials <- lapply(trials, ComputeGazeContact)
  class(trials) <- classes
  trials
}

#' @export
ComputeGazeContact.Trial <- function(trial) {
  looks <- rle(trial$GazeByImageAOI)
  uninterrupted <- looks$lengths[!is.na(looks$values)]
  contact <- round(mean(uninterrupted) * lwl_constants$ms_per_frame)
  trial %@% "AvgGazeContact" <- contact
  trial
}
