\name{TimeSlice}
\alias{TimeSlice}
\title{Slice an interval of time in a Trial or dataframe}
\usage{
TimeSlice(...)
}
\arguments{
  \item{dframe}{a `data.frame` object (i.e., a `Trial` or a
  data-frame describing log-odds) with a column named
  `Time`.}

  \item{trials}{a list of Trial objects}

  \item{from}{the time at which to start slicing. This
  value can be a `character` string naming a valid timing
  attribute of the data-frame or trial (e.g.,
  `"TargetOnset"` for a `Trial`), a numeric value
  specifying a time-point in milliseconds, or `NULL` in
  which case the function slices from the first time
  frame.}

  \item{to}{time to which to finish slicing. The parameter
  may be of the same classes as described above for `from`,
  but when `NULL` is passed a value, the final time frame
  is used for slicing.}
}
\value{
A time-sliced subset of the `dframe` or the `trials`. An
attribute called "NumberOfFrames" is attached to the
updated objects, so the length of the time-sliced dataframe
maybe queried.
}
\description{
Slice an interval of time in a Trial or dataframe
}

