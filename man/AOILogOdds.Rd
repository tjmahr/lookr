\name{AOILogOdds}
\alias{AOILogOdds}
\title{Compute Log-Odds of Looking to Target versus Non-target Areas of Interest.}
\usage{
AOILogOdds(trials, aoi_column = "GazeByImageAOI", target_aoi = c("Target"),
  distractor_aoi = c("SemanticFoil", "PhonologicalFoil", "Unrelated"),
  bin_width = 3)
}
\arguments{
  \item{trials}{A list of `Trial` objects that have been
  aligned (via `AlignTrials`) and had discretized AOI data
  added (via `AddAOIData`).}

  \item{aoi_column}{A character vector whose elements each
  name a column of discretized AOI data in all the `Trial`
  objects in `trials`. Default is `GazeByImageAOI`.}

  \item{target_aoi}{A vector that specifies which AOI codes
  should be counted as looks to target---i.e., which AOI
  codes should be summed as the numerator of the log odds.
  Default is `Target`, which counts only the looks to the
  target image in the `GazeByImageAOI` discretization.}

  \item{distractor_aoi}{Vector that specifies which AOIs
  should be counted as looks to the "distractor" image, or
  perhaps more correctly, looks away from target---i.e.,
  which AOI codes should be summed as the denominator of
  the log odds.}

  \item{bin_width}{An integer specifying the number of
  adjacent frames that should be binned and used to compute
  the log odds of the discretized data.}
}
\value{
A 2--column `data.frame` of binned gaze data enumerating
(1) the mean time of each bin and (2) the log-odds of looks
to each type of discretized AOI data specified by
`aoi_column`.
}
\description{
The value of the log-odds for each time-point is found by:
1. binning (down-sampling) the time-points in each trial;
2. summing all the looks to the "target" AOI in each bin
across all trials in `trials` (call this `target_sum`); 3.
summing all the looks to the "distractor" AOI in each bin
across all trials (call this `distract_sum`); 4. correcting
zero values of `target_sum` and `distract_sum`; 5.
computing `log(target_sum / distract_sum)` for each bin.
}
\details{
The times at which each log-odds is defined are found by
taking the mean time value of each bin. The times and
values of the log odds function(s) are collected into a
`data.frame`. The first column of the `data.frame` is the
times, each other column is a log-odds.
}

