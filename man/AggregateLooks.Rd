\name{AggregateLooks}
\alias{AggregateLooks}
\title{Aggregate looks to target image}
\usage{
AggregateLooks(frame, formula = Subj + Condition + Time ~ GazeByImageAOI)
}
\arguments{
  \item{frame}{a dataframe of melted looking data,
  containing a \code{GazeByImageAOI} column.}

  \item{formula}{a \code{dcast} formula for aggregating the
  looking data. The default is \code{Subj + Condition +
  Time ~ GazeByImageAOI}}
}
\value{
a dataframe with columns of aggregated looks.
}
\description{
The returned dataframe has columns for the number of looks
to the target image (\code{Target}), looks to distractor
image(s) (\code{Others}), number of missing looks
(\code{NAs}), number of tracked looks that don't fall in an
AOI (\code{Elsewhere}), and the proportion of looks to
target versus competing AOIs (\code{Proportion}).
}

