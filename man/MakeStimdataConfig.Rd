\name{MakeStimdataConfig}
\alias{MakeStimdataConfig}
\title{Make a stimdata configuration}
\usage{
MakeStimdataConfig(named_stim, numerics = NULL, constants = NULL,
  derived = NULL)
}
\arguments{
  \item{named_stim}{a character vector of named values that
  specifies which stimdata fields to extract from an Eprime
  log file.}

  \item{numerics}{names of stimuli in \code{named_stim}
  that represent numeric values.}

  \item{constants}{constant values to specify after the
  \code{named_stim} have been extracted.}

  \item{derived}{a character vector of expressions to
  execute once constants have been added to the stimdata
  description.}
}
\value{
a list with the class \code{StimConfig}. This list has four
slots: \code{Stim}, \code{Numerics}, \code{Constants}, and
\code{Derived}.
}
\description{
\code{MakeStimdataConfig} specifies how \code{ExtractStim}
processes an Eprime stimdata file.
}

