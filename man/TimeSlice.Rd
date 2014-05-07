\name{TimeSlice}
\alias{TimeSlice}
\title{Slice an interval of time in a Trial or dataframe}
\usage{
TimeSlice(...)
}
\arguments{
  \item{dframe}{a \code{data.frame} object (i.e., a
  \code{Trial} or a data-frame describing log-odds) with a
  column named \code{Time}.}

  \item{trials}{a list of Trial objects}

  \item{from}{the time at which to start slicing. This
  value can be a \code{character} string naming a valid
  timing attribute of the data-frame or trial (e.g.,
  \code{"TargetOnset"} for a \code{Trial}), a numeric value
  specifying a time-point in milliseconds, or \code{NULL}
  in which case the function slices from the first time
  frame.}

  \item{to}{time to which to finish slicing. The parameter
  may be of the same classes as described above for
  \code{from}, but when \code{NULL} is passed a value, the
  final time frame is used for slicing.}
}
\value{
A time-sliced subset of the \code{dframe} or the
\code{trials}. An attribute called "NumberOfFrames" is
attached to the updated objects, so the length of the
time-sliced dataframe maybe queried.
}
\description{
Slice an interval of time in a Trial or dataframe
}

