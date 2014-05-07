
# Shortcut for reduction
ReduceTrials <- function(trials) {
  trials <- TimeSlice(AlignTrials(trials))  
  trials <- InterpolateMissingFrames(AddAOIData(trials))
  CalculateMistrackings(trials)
}
