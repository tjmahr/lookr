\name{Gazedata}
\alias{Gazedata}
\title{Load and parse a \code{.gazedata} file for a LWL experiment}
\usage{
  Gazedata(gazedata_path, output_file = NULL)
}
\arguments{
  \item{gazedata_path}{Either the full or relative path to
  the \code{.gazedata} file that is to be parsed.}

  \item{output_file}{Either \code{NULL} or the path---full
  or relative---for the output file. If \code{NULL}, then
  no output file is created. If a path is specified, then
  the parsed gazedata is written out as a \code{.csv} file.
  Default is \code{NULL}.}
}
\value{
  A dataframe containing the parsed gazedata. Each row of
  the dataframe contains the gazedata for a single frame of
  time recorded during the experiment. The dataframe has
  the additional class of \code{Gazedata}. The following
  columns are found in the dataframe: \code{Task},
  \code{Subject}, \code{BlockNo}, \code{TrialNo},
  \code{Time}, \code{XLeft}, \code{XRight}, \code{XMean},
  \code{YLeft}, \code{YRight}, \code{YMean}, \code{ZLeft},
  \code{ZRight}, \code{ZMean}, \code{DiameterLeft},
  \code{DiameterRight}, and \code{DiameterMean}.
}
\description{
  Load and parse a \code{.gazedata} file for a LWL
  experiment
}
\details{
  The following columns are captured in the gazedata file:

  \tabular{ll}{ Subject \tab Session \cr ID \tab TrialID
  \cr TETTime \tab RTTime \cr CursorX \tab CursorY \cr
  TimestampSec \tab TimestampMicrosec \cr XGazePosLeftEye
  \tab XGazePosRightEye \cr YGazePosLeftEye \tab
  YGazePosRightEye \cr XCameraPosLeftEye \tab
  XCameraPosRightEye \cr YCameraPosLeftEye \tab
  YCameraPosRightEye \cr DiameterPupilLeftEye \tab
  DiameterPupilRightEye \cr DistanceLeftEye \tab
  DistanceRightEye \cr ValidityLeftEye \tab
  ValidityRightEye \cr Target \tab Stimulus }

  We extract the columns the following columns:
  \code{TrialId}, \code{RTTime}, \code{XGazePosLeftEye},
  \code{XGazePosRightEye}, \code{YGazePosLeftEye},
  \code{YGazePosRightEye}, \code{DistanceLeftEye},
  \code{DistanceRightEye}, \code{DiameterPupilLeftEye} and
  \code{DiameterPupilRightEye}.

  Once these column values are loaded, we make three
  modifications to the gazedata.

  \enumerate{ \item Gaze values are defined in screen
  proportions. Values that fall outside [0,1] are outside
  of the boundaries of the screen and therefore are
  nonsensical. Replace them with \code{NA}. We perform a
  similar correction on negative pupil diameters and
  eye-distances by replacing those negative values with
  \code{NA}.

  \item The origin of the screen is the upper-left-hand
  corner of the screen.  Flip the y-values so that the
  origin is in a more familiar position in the
  lower-left-hand corner of the screen. This way, low y
  values are closer to the bottom of the screen.

  \item Compute the mean x, y, distance and diameter values
  for the left and right eyes. \code{NA} values are ignored
  when computing the mean, so the pair \code{(XLeft = NA,
  XRight = .5)} yields \code{XMean = .5}. }

  Information about the task, block number, and subject id
  are extracted from the basename of the gazedata file.
  This function expects the gazedata file to have the
  format \code{[Task]_[BlockNo]_[SubjectID].gazedata}.

  Information about the stimuli for each trial is extracted
  from the associated \code{.txt} file that is output by
  E-prime. These values are handled by the \code{Stimdata}
  function.
}
\examples{
gaze_path <- "inst/tests/data/Coartic_WFFArea_1a/302P22MS1/Coartic_Block1_302P22MS1.gazedata"
gaze <- Gazedata(gaze_path)
names(gaze)
# [1] "Task"          "Subject"       "BlockNo"       "TrialNo"       "Time"
# [6] "XLeft"         "XRight"        "XMean"         "YLeft"         "YRight"
#[11] "YMean"         "ZLeft"         "ZRight"        "ZMean"         "DiameterLeft"
#[16] "DiameterRight" "DiameterMean"
}

