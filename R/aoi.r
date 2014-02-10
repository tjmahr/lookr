
# Create a generic function to add AOI information to trials
AddAOIData <- function(...) UseMethod('AddAOIData')

AddAOIData.list <- function(trials) {
  # Preserve classes of inputted list
  classes <- class(trials)
  trials <- lapply(trials, AddAOIData)
  class(trials) <- classes
  trials
}


#' Transform gazedata coordinates into AOI data
#' 
#' @param trial A Trial object with a `Task` and `Protocol` attributes.
#' @return The Trial object with the discretized data added to it as new
#'   columns.
AddAOIData.Trial <- function(trial) {
  # We use the trials task and protocol attributes to determine what stimuli are
  # contained in the AOI.
  if (trial %@% "Task" == "RWL") {
    stim <- list(Target = "TargetImage", SemanticFoil = "SemanticFoilImage", 
                 PhonologicalFoil = "PhonologicalFoilImage", 
                 Unrelated = "UnrelatedImage")
  } else {
    stim <- list(Target = "TargetImage", Distractor = "DistractorImage")
  }
  
  # The WFF_Movie protocol has an addition AOI.
  if (trial %@% "Protocol" == "WFF_Movie") {
    trial %@% "FixationImage" <- "FixationImage"
    stim <- c(stim, Fixation = "FixationImage")
  }  
  
  # We want two pieces of AOI information:
  # 1. The screen-location of the image where the gaze is fixated 
  #    (e.g., `ImageL` or `UpperLeftImage`)
  # 2. The stimuli contained there (e.g., `TargetImage` or `PhonologicalFoil`)
  
  # By default we set all values as `tracked`---i.e., the gaze falls onscreen
  # but is not in a relevant AOI. 
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




#' Get frames with gazedata within an Area of interest
#' 
#' @param trial a trial with an `XMean` and a `YMean` column.
#' @param img_AOI the coordinates of a rectangular Area of Interest given in
#'   screen proportions.
#' @return a vector of boolean values indicating whether the gaze values fall 
#'   into the AOI at each time point in the trial.
GetFramesWithGazeInAOI <- function(trial, img_AOI) {
  # Break the image AOI into its x- and y-boundaries.
  x_bounds <- img_AOI$x
  y_bounds <- img_AOI$y
  
  # Get the frames whose `XMean` value is within the x-boundaries of the image
  # AOI. Change N`A` values to `FALSE`.
  gaze_in_x_bounds <- x_bounds[1] <= trial$XMean & trial$XMean <= x_bounds[2]
  gaze_in_x_bounds[is.na(gaze_in_x_bounds)] <- FALSE
  
  # Get the frames whose `YMean` value is within the y-boundaries of the image
  # AOI. Change `NA` values to `FALSE`.
  gaze_in_y_bounds <- y_bounds[1] <= trial$YMean & trial$YMean <= y_bounds[2]
  gaze_in_y_bounds[is.na(gaze_in_y_bounds)] <- FALSE
  
  # The frames whose gaze is in the image AOI are those frames whose `XMean`
  # gaze value is in the x-boundaries and whose `YMean` value is in the
  # y-boundaries.
  gaze_in_aoi <- gaze_in_x_bounds & gaze_in_y_bounds
  gaze_in_aoi
} 





#' Get the boundaries of an Area of Interest from its name
#' 
#' @param image_location a string naming an image location. It may be:
#'   `UpperLeftImage`, `UpperRightImage`, `LowerRightImage`, `LowerLeftImage`,
#'   `FixationImage`, `ImageL`, or `ImageR`.
#' @return a list specifying the boundaries of the named AOI
GetImageAOI <- function(image_location) {
  # Stop on non-string input
  if (!is.character(image_location)) stop("Invalid AOI name (not a string)")  
  AOIs <- list(
    # Four image (RWL) tasks
    UpperLeftImage = DefineAOI(x_pix = c(410, 860), y_pix = c(500, 50)), 
    UpperRightImage = DefineAOI(x_pix = c(1060, 1510), y_pix = c(500, 50)),
    LowerRightImage = DefineAOI(x_pix = c(1060, 1510), y_pix = c(1150, 700)), 
    LowerLeftImage = DefineAOI(x_pix = c(410, 860), y_pix = c(1150, 700)), 
    # Wait for fixation tasks
    FixationImage = DefineAOI(x_pix = c(885, 1035), y_pix = c(525, 675)),
    # Two image (MP) tasks
    ImageL = DefineAOI(x_pix = c(100, 700), y_pix = c(300, 900)), 
    ImageR = DefineAOI(x_pix = c(1220, 1820), y_pix = c(300, 900))
  )
  if (!is.element(image_location, names(AOIs))) {
    stop(paste0("Invalid AOI name: \"", image_location, "\""))
  }
  AOIs[[image_location]]
}






#' Convert pixel locations of an image to proportion-on-screen location
#' 
#' DefineAOI is a utility function for converting the pixel locations of 
#' the x- and y-boundaries of an image to screen proportions that can be passed 
#' to functions like AOIData. 
#' 
#' @param x_pix A numeric vector whose elements determine the left and right 
#'   boundaries of the image.  The order is not important, since the lesser 
#'   number is always the left boundary and the greater number, always the right
#'   boundary.
#' @param y_pix A numeric vector whose elements determine the lower and upper
#'   boundaries of the image.  The order is not important. Since the pixel 
#'   origin is the top left corner of the screen, the lesser number is always 
#'   the upper boundary, and the greater number is always the lower boundary.
#' @return A list of the form `list(x=(left_prop, right_prop), y=(lower_prop,
#'   upper_prop))`, where `left_prop`, `right_prop`, `lower_prop`, and
#'   `upper_prop` are the screen proportion of the left, right, lower, and upper
#'   boundaries, respectively.
DefineAOI <- function(x_pix, y_pix) {
  # Get the screen-dimensions, resorting to 1920 x 1200 by default if
  # lwl_constants has not been defined.
  default_width <- !exists("lwl_constants$screen_width")
  default_height <- !exists("lwl_constants$screen_height")
  screen_width <- if (default_width) 1920 else lwl_constants$screen_width
  screen_height <- if (default_height) 1200 else lwl_constants$screen_height
  # Compute AOI boundaries in proportions of the screen. Because the origin is
  # the upper left corner of the screen, min(x_pix) is the left boundary while
  # min(y_pix) is the upper boundary of the AOI.
  left_prop <- min(x_pix) / screen_width
  right_prop <- max(x_pix) / screen_width
  # Subtract from height to flip y-values so origin is in lower left corner.
  lower_prop <- (screen_height - max(y_pix)) / screen_height
  upper_prop <- (screen_height - min(y_pix)) / screen_height
  # Bundle the screen proportions of the x- and y-boundaries in a list.
  x_boundaries <- c(left_prop, right_prop)
  y_boundaries <- c(lower_prop, upper_prop)
  structure(list(x = x_boundaries, y = y_boundaries), class = "AOI")
}








