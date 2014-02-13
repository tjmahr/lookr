\name{AOI}
\alias{AOI}
\title{Convert pixel locations of an image to proportion-on-screen location}
\usage{
AOI(x_pix, y_pix)
}
\arguments{
  \item{x_pix}{A numeric vector whose elements determine
  the left and right boundaries of the image.}

  \item{y_pix}{A numeric vector whose elements determine
  the lower and upper boundaries of the image.}
}
\value{
A list of the form `list(x = (left_prop, right_prop), y =
(lower_prop, upper_prop))`, where `left_prop`,
`right_prop`, `lower_prop`, and `upper_prop` are the screen
proportion of the left, right, lower, and upper boundaries,
respectively.
}
\description{
AOI is a utility function for converting the pixel
locations of the x- and y-boundaries of an image to screen
proportions that can be passed to functions like
AddAOIData.
}
\keyword{internal}

