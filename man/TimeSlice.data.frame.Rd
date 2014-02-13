\name{TimeSlice.data.frame}
\alias{TimeSlice.data.frame}
\title{Slice an interval of time in a Trial or dataframe}
\usage{
\method{TimeSlice}{data.frame}(dframe, from = lwl_opts$get("timeslice_start"),
  to = lwl_opts$get("timeslice_end"))
}
\arguments{
  \item{dframe}{a `data.frame` object (i.e., a `Trial` or a
  data-frame describing log-odds) with a column named
  `Time`.}

  \item{from}{the time at which to start slicing. This
  value can be a `character` string naming a valid timing
  attribute of the data-frame or trial (e.g.,
  `"TargetOnset"` for a `Trial`), a function that returns
  at timing attribute (e.g., `TargetOnset` if
  `TargetOnset(trial)` would return the target onset
  attribute), a numeric value specifying a time-point in
  milliseconds, or `NULL` in which case the function slices
  from the first time frame.}

  \item{to}{time to which to finish slicing. The parameter
  may be of the same classes as described above for `from`,
  but when `NULL` is passed a value, the final time frame
  is used for slicing.}
}
\value{
A time-sliced subset of `dframe`. An attribute called
"NumberOfFrames" is attached to the dataframe, so the
length of the time-sliced dataframe maybe queried.
}
\description{
Slice an interval of time in a Trial or dataframe
}

