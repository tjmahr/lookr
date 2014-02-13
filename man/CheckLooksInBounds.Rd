\name{CheckLooksInBounds}
\alias{CheckLooksInBounds}
\title{Check whether values in a vector each fall between an upper and lower bound}
\usage{
CheckLooksInBounds(xs, lower_bound, upper_bound)
}
\arguments{
  \item{xs}{a set of x or y gaze coordinates}

  \item{lower_bound}{the lower bound of the range to check}

  \item{upper_bound}{the upper bound of the range to check}
}
\value{
a vector of boolean values indicating whether each x in xs
falls between the lower bound and upper bound (inclusive).
NA values in xs are missing looks so they are reported as
FALSE.
}
\description{
Check whether values in a vector each fall between an upper
and lower bound
}
\keyword{internal}

