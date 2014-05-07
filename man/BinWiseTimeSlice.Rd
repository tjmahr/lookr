\name{BinWiseTimeSlice}
\alias{BinWiseTimeSlice}
\title{Extract a subset of trial data, congruent to bin size}
\usage{
BinWiseTimeSlice(trials, start_time, end_time, bin_size = 3)
}
\arguments{
  \item{trials}{a list of trials}

  \item{start_time}{the starting time (ms) of the interval}

  \item{end_time}{the end time (ms) of the interval}

  \item{bin_size}{the number of time frames per bin
  (default is 3)}
}
\value{
a list of \code{trial} objects with the desired interval
extracted and extended so that the number of time frames is
evenly divisisble by the bin size
}
\description{
We only really care about part of each trial, so we should
extract that time interval from each trial and not worry
about the rest of the trial. Our log-odds function however
analyzes AOI data 3 bins at a time (or in 49.9638 ms
chunks), so we should make sure that our interval can be
broken into bins (i.e., broken into 49.9638 ms chunks).
}
\details{
This function extends the time interval for extraction to
the nearest bin. The result is that the number of time
frames in the extracted interval is evenly divisible by the
bin size.
}

