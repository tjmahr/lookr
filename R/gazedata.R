#' Load a \code{.gazedata} file for an experiment
#'
#' Loads \code{.gazedata} file created by an Eprime experiment running on a
#' Tobii eyetracker, and performs typical data reduction on that file.
#'
#' @param gazedata_path path to the \code{.gazedata} file that is to be parsed.
#' @param output_file Whether to write the parsed gazedata to a csv file.
#'   Defaults to FALSE. If TRUE, that gazedata is saved to
#'   \code{[folder]/[basename]_gaze.csv}, where the folder and basename are
#'   extracted from the path used in \code{gazedata_path}
#' @return A dataframe containing the parsed gazedata. Each row of the dataframe
#'   contains the eye-tracking data for a single frame of time recorded during
#'   the experiment. The dataframe has the additional class of \code{Gazedata}.
#' @details
#' The following columns are captured in the gazedata file:
#' \tabular{ll}{
#'    Subject              \tab Session               \cr
#'    ID                   \tab TrialID               \cr
#'    TETTime              \tab RTTime                \cr
#'    CursorX              \tab CursorY               \cr
#'    TimestampSec         \tab TimestampMicrosec     \cr
#'    XGazePosLeftEye      \tab XGazePosRightEye      \cr
#'    YGazePosLeftEye      \tab YGazePosRightEye      \cr
#'    XCameraPosLeftEye    \tab XCameraPosRightEye    \cr
#'    YCameraPosLeftEye    \tab YCameraPosRightEye    \cr
#'    DiameterPupilLeftEye \tab DiameterPupilRightEye \cr
#'    DistanceLeftEye      \tab DistanceRightEye      \cr
#'    ValidityLeftEye      \tab ValidityRightEye      \cr
#'    Target               \tab Stimulus
#' }
#'
#' We extract the columns the following columns: \code{TrialId}, \code{RTTime},
#' \code{XGazePosLeftEye}, \code{XGazePosRightEye}, \code{YGazePosLeftEye},
#' \code{YGazePosRightEye}, \code{DistanceLeftEye}, \code{DistanceRightEye},
#' \code{DiameterPupilLeftEye} and \code{DiameterPupilRightEye}.
#'
#' Once these column values are loaded, we make three modifications to the
#' gazedata.
#'
#' \enumerate{
#'   \item Gaze measurements with \code{Validity} codes greater than or equal to
#'   1 are replaced with NA values.
#'
#'   \item X,Y gaze values are defined in screen proportions. Values that fall
#'   outside [0,1] are outside of the boundaries of the screen and therefore are
#'   nonsensical. Replace them with \code{NA}. We perform a similar correction
#'   on pupil diameters and eye-distances by replacing negative
#'   values with \code{NA}.
#'
#'   \item The origin of the screen is the upper-left-hand corner of the screen.
#'   Flip the y-values so that the origin is in a more familiar position in the
#'   lower-left-hand corner of the screen. This way, low y values are closer to
#'   the bottom of the screen.
#'
#'   \item Compute the mean x, y, distance and diameter values for the left and
#'   right eyes. \code{NA} values are ignored when computing the mean, so the
#'   pair \code{(XLeft = NA, XRight = .5)} yields \code{XMean = .5}.
#' }
#'
#' Information about the task, block number, and subject id are extracted from
#' the basename of the gazedata file. This function expects the gazedata file
#' to have the format \code{[Task]_[BlockNo]_[SubjectID].gazedata}.
#'
#' Information about the stimuli for each trial is extracted from the associated
#' \code{.txt} file that is output by E-prime. These values are handled by the
#' \code{Stimdata} function.
#' @references \href{http://bit.ly/1AtKyhR}{Tobii Toolbox for Matlab: Product
#'   Description & User Guide}
#' @export
Gazedata <- function(gazedata_path, output_file = lwl_opts$get("write_gazedata")) {
  gazedata <- read.delim(gazedata_path, na.strings = c("-1.#INF", "1.#INF"),
                         stringsAsFactors = FALSE)

  # Select/rename columns with experiment information (timing and trial
  # number) and gaze measurements from each eye
  cols_to_keep <- list(
    TrialNo = "TrialId", Time = "RTTime",
    XLeft = "XGazePosLeftEye", XRight = "XGazePosRightEye",
    YLeft = "YGazePosLeftEye", YRight = "YGazePosRightEye",
    ZLeft = "DistanceLeftEye", ZRight = "DistanceRightEye",
    ValidityLeft = "ValidityLeftEye", ValidityRight = "ValidityRightEye",
    DiameterLeft = "DiameterPupilLeftEye",
    DiameterRight = "DiameterPupilRightEye")
  gazedata <- gazedata[unlist(cols_to_keep)]
  gazedata <- setNames(gazedata, names(cols_to_keep))

  # Set some shortcuts
  measures <- c("X", "Y", "Z", "Diameter")
  measures_L <- paste0(measures, "Left")
  measures_R <- paste0(measures, "Right")

  # From the Tobii manual, "Validity codes should be used for data filtering to
  # remove data points that are obviously incorrect. If you export the raw data
  # file, we recommend removing all data points with a validity code of 2 or
  # higher."
  invalid_L <- which(2 <= gazedata$ValidityLeft)
  invalid_R <- which(2 <= gazedata$ValidityRight)
  gazedata[invalid_L, measures_L] <- NA
  gazedata[invalid_R, measures_R] <- NA

  # Replace all values of gazedata that fall beyond [0, 1] (offscreen) with NA.
  correct_offscreen_gazes <- function(gaze) {
    ifelse(gaze < 0 | 1 < gaze, NA, gaze)
  }
  screen_cols <- c("XLeft", "XRight", "YLeft", "YRight")
  gazedata[screen_cols] <- colwise(correct_offscreen_gazes)(gazedata[screen_cols])

  # Correct values of gazedata that cannot be negative (distances, diameters)
  correct_distances <- function(gaze) {
    ifelse(gaze < 0, NA, gaze)
  }
  distances <- c("ZLeft", "ZRight", "DiameterLeft", "DiameterRight")
  gazedata[distances] <- colwise(correct_distances)(gazedata[distances])

  # Flip the y values.
  gazedata <- mutate(gazedata, YLeft = 1 - YLeft, YRight = 1 - YRight)

  # A "monocular mean" averages both eyes together. If data is available in just
  # one eye, use the available value as the mean.
  compute_monocular_mean <- function(x1, x2) {
    xm <- rowMeans(cbind(x1, x2), na.rm = TRUE)
    # NaN => NA
    ifelse(is.nan(xm), NA, xm)
  }

  gazedata <- mutate(gazedata,
    XMean = compute_monocular_mean(XLeft, XRight),
    YMean = compute_monocular_mean(YLeft, YRight),
    ZMean = compute_monocular_mean(ZLeft, ZRight),
    DiameterMean = compute_monocular_mean(DiameterLeft, DiameterRight)
  )

  # Add informative columns from the gazedata filename
  file_info <- ParseFilename(gazedata_path)
  gazedata$Task <- file_info$Task
  gazedata$BlockNo <- file_info$Block
  gazedata$Subject <- file_info$Subject
  gazedata$Basename <- file_info$Basename

  # Re-order the columns of gazedata.
  cols_in_order <- c("Task", "Subject", "BlockNo", "Basename", "TrialNo",
                     "Time", "XLeft", "XRight", "XMean", "YLeft", "YRight",
                     "YMean", "ZLeft", "ZRight", "ZMean", "DiameterLeft",
                     "DiameterRight", "DiameterMean")
  gazedata <- gazedata[cols_in_order]

  # Optionally write out gazedata as a csv
  if (output_file) {
    landing_dir <- dirname(gazedata_path)
    landing_file <- paste0(file_info$Basename, "_gaze.csv")
    landing_path <- file.path(landing_dir, landing_file)
    write.csv(gazedata, landing_path, row.names = FALSE)
  }

  as.Gazedata(gazedata)
}




#' Extract information from a filename
#'
#' The basename of a file in a Looking While Listening task conforms to the
#' naming convention: [Task]_[BlockNo]_[SubjectID]. Block names are reduced to
#' just the integer value, i.e., \code{"Block1"} becomes \code{1}.
#'
#' @param filename a filename with a pattern like [Task]_[BlockNo]_[SubjectID]
#' @return a list with \code{Task}, \code{Block},\code{Subject} and
#'   \code{Basename} fields.
#'
#' @export
ParseFilename <- function(filename) {
  file_basename <- file_path_sans_ext(basename(filename))

  # Extract the fields from the basename.
  file_info <- unlist(str_split(file_basename, pattern = "_"))
  task <- file_info[1]
  block_name <- str_extract(file_basename, "Block[0-9]{1}")

  # `block_name` is "Block1" or "Block2" right now. We just want the number.
  block <- as.integer(str_extract(block_name, pattern = "[1-9]"))

  # The [MFX] field includes X to match the files in the dummy/test data
  subject <- str_extract(file_basename, lwl_constants$l2t_subject)

  # Bundle these four data together
  file_info <- list(Task = task, Block = block, Subject = subject,
                    Basename = file_basename)
  file_info
}
