
#' Shortcut for reduction
#' @export
ReduceTrials <- function(trials) {
  trials <- TimeSlice(AdjustTimes(trials))
  trials <- InterpolateMissingFrames(AddAOIData(trials))
  CalculateMistrackings(trials)
}
