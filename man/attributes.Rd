\name{attributes}
\alias{\%@\%.default}
\alias{\%@\%.list}
\alias{\%@\%<-}
\alias{attributes}
\title{Get or set attributes of an object}
\usage{
  %@%(x, attribute)

  %@%.default(x, attribute)

  %@%.list(xs, attribute)

  %@%(object, attribute, value) <- value
}
\arguments{
  \item{x}{An object with attributes}

  \item{xs}{A list of objects (with attributes).}

  \item{attribute}{A character string that names an
  attribute of the object.}
}
\value{
  The value in the attribute slot of object, or the values
  of the attribute for each element element in a list.
}
\description{
  \code{\%@\%} is an infix-form wrapper for the \code{attr}
  function.
}
\details{
  \code{\%@\%} is a utility function for getting the value
  of an attribute from an R object. This is a wrapper for
  the built-in function \code{attr}. \code{\%@\%.list}
  applies \code{\%@\%} onto each object in a list of R
  objects. Therefore, \code{\%@\%} cannot grab attributes
  of lists, unless they are lists within lists. This is
  weird; maybe I should fix this.
}
\examples{
MakeCat <- function(name, owner) {
 structure(list(Name = name), Owner = owner, class = "cat")
}

kiki <- MakeCat("Kiki", "AML")
nooper <- MakeCat("Nooper", "TJM")

kiki \%@\% "Owner"
# [1] "AML"

list(nooper, kiki) \%@\% "Owner"
# [1] "TJM" "AML"

nooper \%@\% "Owner" <- NA
list(nooper, kiki) \%@\% "Owner"
# [1] NA    "AML"
}

