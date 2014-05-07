

#' Transform gazedata coordinates into AOI data
#' 
#' AddAOIData() adds two new columns to the gazedata in a Trial object: (1) 
#' \code{GazeByAOI}, the screen-location of the image where the gaze is fixated 
#' (e.g., \code{ImageL} or \code{UpperLeftImage}) and (2) \code{GazeByImageAOI},
#' the stimulus contained in the image where the gaze is fixated (e.g.,
#' \code{TargetImage} or \code{PhonologicalFoil}).
#' 
#' If the gaze is tracked but not fixated to a particular image on the screen, 
#' it gets the default value of \code{tracked}. If the gaze is missing during a 
#' particular frame, the value is \code{NA}.
#' 
#' "AOI" stands for Area of Interest.
#' 
#' @export
#' @param trial A Trial object with a \code{Task} and \code{Protocol}
#'   attributes.
#' @param trials a list of Trial objects, each with \code{Task} and
#'   \code{Protocol} attributes.
#' @return The Trial object with the AOI data attached as new columns.
AddAOIData <- function(...) UseMethod('AddAOIData')

#' @export
AddAOIData.list <- function(trials) {
  classes <- class(trials)
  trials <- lapply(trials, AddAOIData)
  class(trials) <- classes
  trials
}

#' @export
AddAOIData.Trial <- function(trial) {
  stim <- DetermineAOIStim(trial)
  
  # By default we set all values as `tracked`.
  number_of_frames <- nrow(trial)
  gaze_by_image_aoi <- rep("tracked", times = number_of_frames)
  gaze_by_aoi <- rep("tracked", times = number_of_frames)
  
  # If the gaze is in a specific AOI, overwrite `tracked` in the AOI location
  # and AOI stimuli vectors.
  for (i in seq_along(stim)) {
    stim_img <- stim[[i]]
    stim_name <- names(stim)[i]
    location <- trial %@% stim_img
    
    img_AOI <- GetImageAOI(location)
    gaze_at_location <- GetFramesWithGazeInAOI(trial, img_AOI)
    gaze_by_image_aoi[gaze_at_location] <- stim_name
    gaze_by_aoi[gaze_at_location] <- location
  }
  
  # Overwrite `tracked` with `NA` when the x or y gaze values were not tracked.
  gaze_mistracked <- is.na(trial$XMean) | is.na(trial$YMean)
  gaze_by_image_aoi[gaze_mistracked] <- NA
  gaze_by_aoi[gaze_mistracked] <- NA
  
  # Add the AOI columns to the trial
  trial$GazeByAOI <- gaze_by_aoi
  trial$GazeByImageAOI <- gaze_by_image_aoi
  trial
}


# Use task and protocol attributes to determine which stimuli and AOI are used. 
DetermineAOIStim <- function(trial) {
  stim <- if (trial %@% "Task" == "RWL") {
    list(Target = "TargetImage", SemanticFoil = "SemanticFoilImage", 
         PhonologicalFoil = "PhonologicalFoilImage", 
         Unrelated = "UnrelatedImage")
  } else {
    list(Target = "TargetImage", Distractor = "DistractorImage")
  }
  
  # The WFF_Movie protocol has an additional AOI.
  if (trial %@% "Protocol" == "WFF_Movie") {
    trial %@% "FixationImage" <- "FixationImage"
    stim <- c(stim, Fixation = "FixationImage")
  }
  stim
}








#' Get frames with gazedata within an Area of interest
#' 
#' @keywords internal
#' @param trial a Trial with \code{XMean} and \code{YMean} columns.
#' @param img_AOI the coordinates of a rectangular Area of Interest given in 
#'   screen proportions.
#' @return a vector of boolean values indicating whether the gaze values fall 
#'   into the AOI at each time point in the trial.
GetFramesWithGazeInAOI <- function(trial, img_AOI) {
  # Break the image AOI into its x- and y-boundaries.
  x_bounds <- img_AOI$x
  y_bounds <- img_AOI$y
  
  # The frames whose gaze is in the image AOI are those frames whose `XMean`
  # gaze value is in the x-boundaries and whose `YMean` value is in the
  # y-boundaries.
  gaze_in_x_bounds <- CheckLooksInBounds(trial$XMean, x_bounds[1], x_bounds[2])
  gaze_in_y_bounds <- CheckLooksInBounds(trial$YMean, y_bounds[1], y_bounds[2])
  gaze_in_aoi <- gaze_in_x_bounds & gaze_in_y_bounds
  gaze_in_aoi
} 


#' Check whether values in a vector each fall between an upper and lower bound
#' 
#' @keywords internal
#' @param xs a set of x or y gaze coordinates
#' @param lower_bound the lower bound of the range to check
#' @param upper_bound the upper bound of the range to check
#' @return a vector of boolean values indicating whether each x in \code{xs} 
#'   falls between the lower bound and upper bound (inclusive). \code{NA} values
#'   in \code{xs} are missing looks so they are reported as \code{FALSE}.
CheckLooksInBounds <- function(xs, lower_bound, upper_bound) {
  gaze_in_bounds <- lower_bound <= xs & xs <= upper_bound
  gaze_in_bounds[is.na(gaze_in_bounds)] <- FALSE
  gaze_in_bounds
}


#' Get the boundaries of an Area of Interest from its name
#' 
#' @keywords internal
#' @param image_location a string naming an image location. It may be: 
#'   \code{UpperLeftImage}, \code{UpperRightImage}, \code{LowerRightImage},
#'   \code{LowerLeftImage}, \code{FixationImage}, \code{ImageL}, or
#'   \code{ImageR}.
#' @return a list specifying the boundaries of the named AOI
GetImageAOI <- function(image_location) {
  # Stop on non-string input
  if (!is.character(image_location)) stop("Invalid AOI name (not a string)")  
  
  AOIs <- list(
    # Four image (RWL) tasks
    UpperLeftImage = AOI(x_pix = c(410, 860), y_pix = c(500, 50)), 
    UpperRightImage = AOI(x_pix = c(1060, 1510), y_pix = c(500, 50)),
    LowerRightImage = AOI(x_pix = c(1060, 1510), y_pix = c(1150, 700)), 
    LowerLeftImage = AOI(x_pix = c(410, 860), y_pix = c(1150, 700)), 
    # Wait for fixation tasks
    FixationImage = AOI(x_pix = c(885, 1035), y_pix = c(525, 675)),
    # Two image (MP) tasks
    ImageL = AOI(x_pix = c(100, 700), y_pix = c(300, 900)), 
    ImageR = AOI(x_pix = c(1220, 1820), y_pix = c(300, 900))
  )
  
  if (!is.element(image_location, names(AOIs))) {
    stop(paste0("Invalid AOI name: \"", image_location, "\""))
  }
  AOIs[[image_location]]
}




#' Convert pixel locations of an image to proportion-on-screen location
#' 
#' AOI is a utility function for converting the pixel locations of the x- and 
#' y-boundaries of an image to screen proportions that can be passed to 
#' functions like AddAOIData.
#' 
#' @keywords internal
#' @param x_pix A numeric vector whose elements determine the left and right 
#'   boundaries of the image.
#' @param y_pix A numeric vector whose elements determine the lower and upper 
#'   boundaries of the image.
#' @return A list of the form \code{list(x = (left_prop, right_prop), y = 
#'   (lower_prop, upper_prop))}, where \code{left_prop}, \code{right_prop}, 
#'   \code{lower_prop}, and \code{upper_prop} are the screen proportion of the 
#'   left, right, lower, and upper boundaries, respectively.
AOI <- function(x_pix, y_pix) {
  width <- lwl_constants$screen_width
  height <- lwl_constants$screen_height
  # Compute AOI boundaries in proportions of the screen. Because the origin is
  # the upper left corner of the screen, min(x_pix) is the left boundary while
  # min(y_pix) is the upper boundary of the AOI.
  left_prop <- min(x_pix) / width
  right_prop <- max(x_pix) / width
  # Subtract from height to flip y-values so origin is in lower left corner.
  lower_prop <- (height - max(y_pix)) / height
  upper_prop <- (height - min(y_pix)) / height
  # Bundle the screen proportions of the x- and y-boundaries in a list.
  x_boundaries <- c(left_prop, right_prop)
  y_boundaries <- c(lower_prop, upper_prop)
  structure(list(x = x_boundaries, y = y_boundaries), class = "AOI")
}
