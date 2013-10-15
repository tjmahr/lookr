\name{FetchAttr}
\alias{FetchAttr}
\title{Curry the attribute fetcher function for specific attribute}
\usage{
  FetchAttr(attribute)
}
\arguments{
  \item{attribute}{a character string naming an attribute}

  \item{object}{in the returned function, the object whose
  attributes we query.}
}
\value{
  a function that returns the value of `attribute`
}
\description{
  Originally, one had to write the several functions to
  retrieve some attribute of a trial or set of trials.
  E.g., getting the `WordGroup` attribute required:
}
\details{
  ``` WordGroup <- function(x, ...) UseMethod('WordGroup')
  WordGroup.Trial <- function(trial) {attr(trial,
  'WordGroup')} WordGroup.list <- function(list)
  {mapply(WordGroup, list)} ```

  The `%@%` methods by Pat Reidy obviated this problem by
  providing a general function for a getting/setting
  attributes from trials or lists of trials. Named
  functions like `StimType(...)` or `WordGroup(...)` are
  still useful and may be lurking in older eye-tracking
  scripts. The `FetchAttr` is a closure function that
  returns a named function for retrieving a specified
  attribute.
}

