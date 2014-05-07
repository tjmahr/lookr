\name{AggregateLooks}
\alias{AggregateLooks}
\title{Aggregate looks to target image}
\usage{
AggregateLooks(frame, formula = Subj + Condition + Time ~ GazeByImageAOI)
}
\arguments{
  \item{frame}{a dataframe of melted looking data,
  containing a `GazeByImageAOI` column.}

  \item{formula}{a `dcast` formula for aggregating the
  looking data. The default is `Subj + Condition + Time ~
  GazeByImageAOI`}
}
\value{
a dataframe with columns of aggregated looks.
}
\description{
The returned dataframe has columns for the number of looks
to the target image (`Target`), looks to distractor
image(s) (`Others`), number of missing looks (`NAs`),
number of tracked looks that don't fall in an AOI
(`Elsewhere`), and the proportion of looks to target versus
competing AOIs (`Proportion`).
}

