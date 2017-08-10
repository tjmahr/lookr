
#' Reduce trial data using conventional steps and default options
#' @param trials a TrialList
#' @return the trials with times adjusted, time-sliced to interval of an
#'   interest, AOIs added, interpolated frames, and missing data calculated.
#' @export
ReduceTrials <- function(trials) {
  trials <- TimeSlice(AdjustTimes(trials))
  trials <- InterpolateMissingFrames(AddAOIData(trials))
  CalculateMistrackings(trials)
}
