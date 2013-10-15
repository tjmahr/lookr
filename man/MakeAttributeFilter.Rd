\name{MakeAttributeFilter}
\alias{MakeAttributeFilter}
\title{Make a function to filter out trials based on attribute values}
\usage{
  MakeAttributeFilter(attr_name)
}
\arguments{
  \item{attr_name}{the name of attribute to filter by}

  \item{trials}{a list of Trial objects}

  \item{attr_value}{a vector of values to filter out}
}
\value{
  a function `f(trials, attr_values)` that filters out
  trials with the specified `attr_values` for the given
  `attr_name`.
}
\description{
  This is a curried function.
}

