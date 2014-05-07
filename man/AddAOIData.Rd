\name{AddAOIData}
\alias{AddAOIData}
\title{Transform gazedata coordinates into AOI data}
\usage{
AddAOIData(...)
}
\arguments{
  \item{trial}{A Trial object with a \code{Task} and
  \code{Protocol} attributes.}

  \item{trials}{a list of Trial objects, each with
  \code{Task} and \code{Protocol} attributes.}
}
\value{
The Trial object with the AOI data attached as new columns.
}
\description{
AddAOIData() adds two new columns to the gazedata in a
Trial object: (1) \code{GazeByAOI}, the screen-location of
the image where the gaze is fixated (e.g., \code{ImageL} or
\code{UpperLeftImage}) and (2) \code{GazeByImageAOI}, the
stimulus contained in the image where the gaze is fixated
(e.g., \code{TargetImage} or \code{PhonologicalFoil}).
}
\details{
If the gaze is tracked but not fixated to a particular
image on the screen, it gets the default value of
\code{tracked}. If the gaze is missing during a particular
frame, the value is \code{NA}.

"AOI" stands for Area of Interest.
}

