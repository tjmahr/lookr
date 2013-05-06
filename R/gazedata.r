









#' Load and parse a \code{.gazedata} file for a LWL experiment
#' 
#' @param gazedata_path Either the full or relative path to the \code{.gazedata}
#'   file that is to be parsed.
#' @param output_file Either \code{NULL} or the path---full or relative---for 
#'   the output file. If \code{NULL}, then no output file is created. If a path 
#'   is specified, then the parsed gazedata is written out as a \code{.csv}
#'   file. Default is \code{NULL}.
#' @return A dataframe containing the parsed gazedata. Each row of the dataframe
#'   contains the gazedata for a single frame of time recorded during the 
#'   experiment. The dataframe has the additional class of \code{Gazedata}. The 
#'   following columns are found in the dataframe: \code{Task}, \code{Subject}, 
#'   \code{BlockNo}, \code{TrialNo}, \code{Time}, \code{XLeft}, \code{XRight}, 
#'   \code{XMean}, \code{YLeft}, \code{YRight}, \code{YMean}, \code{ZLeft}, 
#'   \code{ZRight}, \code{ZMean}, \code{DiameterLeft}, \code{DiameterRight}, and
#'   \code{DiameterMean}.
#'   
#' @details
#' The following columns are captured in the gazedata file:
#' 
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
#'   \item Gaze values are defined in screen proportions. Values that fall
#'   outside [0,1] are outside of the boundaries of the screen and therefore are 
#'   nonsensical. Replace them with \code{NA}. We perform a similar correction 
#'   on negative pupil diameters and eye-distances by replacing those negative 
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
#' 
#' @examples
#' gaze_path <- "inst/tests/data/Coartic_WFFArea_1a/302P22MS1/Coartic_Block1_302P22MS1.gazedata"
#' gaze <- Gazedata(gaze_path)
#' names(gaze)
#' # [1] "Task"          "Subject"       "BlockNo"       "TrialNo"       "Time"         
#' # [6] "XLeft"         "XRight"        "XMean"         "YLeft"         "YRight"       
#' #[11] "YMean"         "ZLeft"         "ZRight"        "ZMean"         "DiameterLeft" 
#' #[16] "DiameterRight" "DiameterMean" 
#' 
#' @importFrom tools file_path_sans_ext
#' @import stringr
#' @export
Gazedata <- function(gazedata_path, output_file = NULL) {
  # Read in the .gazedata file.
  gazedata <- read.delim(gazedata_path, na.strings = c('-1.#INF', '1.#INF'),
                         stringsAsFactors = FALSE)
  
  # The only data that needs to be kept from the .gazedata file are those data
  # that log the trial number of the frame, the time of the frame, and the gaze 
  # location for that frame. We also capture pupil diameter, in case future 
  # experiments use this information, as well as the distance of each from the 
  # screen. Therefore:
  columns_to_keep <- c('TrialId', 'RTTime',
                       'XGazePosLeftEye', 'XGazePosRightEye',
                       'YGazePosLeftEye', 'YGazePosRightEye',
                       'DistanceLeftEye', 'DistanceRightEye',
                       'DiameterPupilLeftEye', 'DiameterPupilRightEye')
  gazedata <- gazedata[, columns_to_keep]
  # "Z" denotes the distance of the eye from the screen in mm.
  names(gazedata) <- c('TrialNo', 'Time', 'XLeft', 'XRight', 'YLeft', 'YRight',
                       'ZLeft', 'ZRight', 'DiameterLeft', 'DiameterRight')
  
  # Replace all values of gazedata that fall outside of [0, 1] with NA.
  .CorrectInvalidGazes <- function(gaze) {
    gaze[(gaze < 0) | (gaze > 1)] <- NA
    gaze
  }
  
  gazedata <- within(gazedata, {
    XLeft  <- .CorrectInvalidGazes(XLeft)
    XRight <- .CorrectInvalidGazes(XRight)
    YLeft  <- .CorrectInvalidGazes(YLeft)
    YRight <- .CorrectInvalidGazes(YRight)
    # Correct negative distances
    ZLeft[ZLeft < 0] <- NA
    ZRight[ZRight < 0] <- NA
    # Correct negative pupil diameters
    DiameterLeft[DiameterLeft < 0] <- NA
    DiameterRight[DiameterRight < 0] <- NA
  })
  
  # Flip the y values.
  gazedata <- transform(gazedata, YLeft = 1 - YLeft, YRight = 1 - YRight)
  
  # Compute the mean gaze values.
  ComputePairMeans <- function(x1, x2) rowMeans(cbind(x1, x2), na.rm = TRUE)    
  gazedata <- within(gazedata, {
    XMean <- ComputePairMeans(XLeft, XRight)
    YMean <- ComputePairMeans(YLeft, YRight)
    ZMean <- ComputePairMeans(ZLeft, ZRight)
    DiameterMean <- ComputePairMeans(DiameterLeft, DiameterRight)
  })
  
  # Add informative columns
  
  # The basename of a gazedata file for a Looking While Listening task conforms
  # to the naming convention: `[Task]_[BlockNo]_[SubjectID]`. We extract these
  # 3 fields and include them in the gazedata.
  
  # Get the basename of the gazedata filepath.
  file_name <- basename(gazedata_path)
  file_basename <- file_path_sans_ext(file_name)
  
  # Extract the fields from the basename.
  file_info <- unlist(str_split(file_basename, pattern = "_"))
  task <- file_info[1]
  block_name <- file_info[2]
  subject <- file_info[3]
  
  # `block_name` is "Block1" or "Block2" right now. We just want the number.
  block <- as.integer(str_extract(block_name, pattern = "[1-9]"))
  
  # Include task, block, and subject info. 
  gazedata$Task <- task
  gazedata$BlockNo <- block
  gazedata$Subject <- subject

  # Final steps
  
  # Re-order the columns of gazedata.
  columns_in_order <- c('Task', 'Subject', 'BlockNo', 'TrialNo', 'Time',
                        'XLeft', 'XRight', 'XMean', 'YLeft', 'YRight', 'YMean', 
                        'ZLeft', 'ZRight', 'ZMean', 'DiameterLeft', 
                        'DiameterRight', 'DiameterMean')
  gazedata <- gazedata[, columns_in_order]
  
  # Augment the class of gazedata.
  class(gazedata) <- c('Gazedata', 'data.frame')
  
  # Optionally write out the gazedata data frame.
  if (!is.null(output_file)) {
    write.table(gazedata, file = output_file, sep = ',',
                quote = FALSE, row.names = FALSE)
  }
  
  gazedata
}


