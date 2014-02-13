\name{GetFrameAtTime}
\alias{GetFrameAtTime}
\title{Get the frame number for a certain Time value in a list of Trials}
\usage{
GetFrameAtTime(trials, time_point = 0)
}
\arguments{
  \item{trials}{a list of `Trial` objects}

  \item{time_point}{a numeric value indicating the time to
  find in the Trial}
}
\value{
a numeric vector listing the frames at which the desired
time occurs in each `Trial`
}
\description{
Get the frame number for a certain Time value in a list of
Trials
}

