\name{TimeSlice.list}
\alias{TimeSlice.list}
\title{Slice an interval of time across a list of dataframes}
\usage{
\method{TimeSlice}{list}(trials, from = lwl_opts$get("timeslice_start"),
  to = lwl_opts$get("timeslice_end"))
}
\description{
This function applies the `TimeSlice.data.frame` across a
list of objects and preserves the classes of the `list`
object.
}

