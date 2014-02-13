\name{DefineAOI}
\alias{DefineAOI}
\title{Convert pixel locations of an image to proportion-on-screen location}
\usage{
DefineAOI(x_pix, y_pix)
}
\arguments{
  \item{x_pix}{A numeric vector whose elements determine
  the left and right boundaries of the image.  The order is
  not important, since the lesser number is always the left
  boundary and the greater number, always the right
  boundary.}

  \item{y_pix}{A numeric vector whose elements determine
  the lower and upper boundaries of the image.  The order
  is not important. Since the pixel origin is the top left
  corner of the screen, the lesser number is always the
  upper boundary, and the greater number is always the
  lower boundary.}
}
\value{
A list of the form `list(x=(left_prop, right_prop),
y=(lower_prop, upper_prop))`, where `left_prop`,
`right_prop`, `lower_prop`, and `upper_prop` are the screen
proportion of the left, right, lower, and upper boundaries,
respectively.
}
\description{
DefineAOI is a utility function for converting the pixel
locations of the x- and y-boundaries of an image to screen
proportions that can be passed to functions like AOIData.
}
\keyword{internal}

