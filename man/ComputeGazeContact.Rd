\name{ComputeGazeContact}
\alias{ComputeGazeContact}
\title{Compute the average duration of tracked gazes}
\usage{
ComputeGazeContact(...)
}
\arguments{
  \item{trial}{a Trial object with a `GazeByImageAOI`
  column}

  \item{trials}{a list of Trial objects, each with a
  `GazeByImageAOI` column}
}
\value{
the Trial or list of Trials updated with a new attribute
`AvgGazeContact`
}
\description{
To quantify "flicker" in eye-tracking data, Wass, Smith and
Johnson (2013) found the average duration of tracked
segments of eye-tracking data. This function performs this
calculation, updating trials with a new attribute
`AvgGazeContact`. This value measures the average duration
(ms) of each segment of uninterrupted eye-tracking data in
a trial.
}

