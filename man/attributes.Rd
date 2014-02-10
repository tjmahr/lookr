\name{attributes}
\alias{\%@\%.default}
\alias{\%@\%.list}
\alias{\%@\%<-}
\alias{attributes}
\title{Get or set attributes of an object}
\usage{
x \%@\% attribute

\method{\%@\%}{default}(x, attribute)

\method{\%@\%}{list}(xs, attribute)

x \%@\% attribute <- value
}
\arguments{
  \item{x}{An object with attributes}

  \item{xs}{A list of objects (with attributes).}

  \item{attribute}{A character string that names an
  attribute of the object.}

  \item{value}{a new value for an attribute}
}
\value{
The value in the attribute slot of object, or the values of
the attribute for each element element in a list.
}
\description{
\code{\%@\%} is an infix-form wrapper for the \code{attr}
function.
}
\details{
A \code{Trial} object is a data-frame of looking data, and
metadata about the trial like the images onscreen or
duration of the carrier phrase are stored in attributes
attached to the data-frame. The functions described below
making working with attributes a little easier.

\code{\%@\%} is a utility function for getting the value of
an attribute from an R object. This is a wrapper for the
built-in function \code{attr}. \code{\%@\%.list} applies
\code{\%@\%} onto each object in a list of R objects.
Therefore, \code{\%@\%} cannot grab attributes of lists,
unless they are lists within lists. This is weird; maybe I
should fix this.

Attributes of single objects can be set can be set using
\code{object \%@\% attribute <- value}.
}
\examples{
Cat <- function(name, owner) {
 structure(list(Name = name), Owner = owner, class = "cat")
}

kiki <- Cat("Kiki", "AML")
nooper <- Cat("Nooper", "TJM")

kiki \%@\% "Owner"
# [1] "AML"

list(nooper, kiki) \%@\% "Owner"
# [1] "TJM" "AML"

# Setting/overwriting a attribute
nooper \%@\% "Owner" <- NA
list(nooper, kiki) \%@\% "Owner"
# [1] NA    "AML"
}

