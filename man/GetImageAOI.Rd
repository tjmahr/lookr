\name{GetImageAOI}
\alias{GetImageAOI}
\title{Get the boundaries of an Area of Interest from its name}
\usage{
GetImageAOI(image_location)
}
\arguments{
  \item{image_location}{a string naming an image location.
  It may be: `UpperLeftImage`, `UpperRightImage`,
  `LowerRightImage`, `LowerLeftImage`, `FixationImage`,
  `ImageL`, or `ImageR`.}
}
\value{
a list specifying the boundaries of the named AOI
}
\description{
Get the boundaries of an Area of Interest from its name
}
\keyword{internal}

