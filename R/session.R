#' Create a Session from multiple LWL Blocks
#'
#' \code{Session()} is a generic function for initializing a Session object,
#' which represents the gazedata and stimdata for each trial in an experimental
#' session. The representation of each trial in a Session object is the same as
#' in a Block object.
#'
#' \code{Session.character} creates a Session object from the path of the
#' directory that contains the \code{.gazedata} and \code{.txt} (stimdata) files
#' for blocks of an experimental session.

#' \code{Session.list} is a (largely internal) method for initializing a Session
#' object from a list of Block objects.
#'
#' @param session_path The directory path of both pairs of .gazedata and .txt
#'   files for the session---one pair for each block.
#' @param list_of_blocks A list of Block objects, each Block object a block of
#'   the same experimental session. The Block objects in list_of_blocks should
#'   be ordered as they were presented during the experiment. That is, Block 1
#'   of the experiment should be the first element of list_of_blocks, etc.
#' @return A Session object.
#' @export
Session <- function(...) UseMethod('Session')

#' @export
Session.character <- function(session_path) {
  # Get all the .gazedata files that are in the session directory.
  gazedata_files <- dir(session_path, pattern = 'gazedata', full.names = TRUE)
  if (length(gazedata_files) == 0) {
    stop(paste0("No `.gazedata` files found in ", session_path))
  }
  # Strip off the .gazedata extensions to get the Block basenames
  block_paths <- file_path_sans_ext(gazedata_files)
  # Load each Block then pass them onto Session.list
  blocks <- lapply(block_paths, Block)
  Session(blocks)
}

#' @export
Session.list <- function(blocks) {
  # Create a Session object by concatenating all of the blocks.
  do.call(c, blocks)
}


#' Combine gaze- and stimdata for an experimental Block
#'
#' Block is a generic function for initializing a Block object, which represents
#' the gazedata and stimdata for each trial in an experimental block. Each trial
#' is represented as a data.frame of the gazedata with attributes for the
#' stimdata. The Block object is a list of trials.
#'
#' Block.Gazedata is a method for initializing a Block object from a Gazedata
#' object and a Stimdata object.
#'
#' Block.character is a function for initializing a Block object from the full
#' file path of the block, excluding its extension (since the extension is
#' different for the stimdata and gazedata files).
#'
#' @param block_path A character string. The full file path of the gazedata and
#'   stimdata files (which should have the same directory path and basename)
#'   without the file extensions.
#' @param gazedata A Gazedata object.
#' @param stimdata A Stimdata object.
#' @return A Block object---i.e., a list of trials, where each trial is
#'   represented as a data.frame of its gazedata, augmented with attributes for
#'   the stimdata.
#' @export
Block <- function(...) UseMethod('Block')

#' @export
Block.character <- function(block_path) {
  gazedata <- Gazedata(paste0(block_path, '.gazedata'))
  stimdata <- Stimdata(paste0(block_path, '.txt'))
  Block(gazedata, stimdata)
}

#' @export
Block.Gazedata <- function(gazedata, stimdata) {
  # For each trial in the block, combine the gazedata and stimdata.
  trials <- lapply(stimdata$TrialNo, CombineGazedataStimdata(gazedata, stimdata))
  as.Block(trials)
}




#' Combine Gazedata and Stimdata
#'
#' CombineGazedataStimdata is a utility function for combining the gazedata and
#' stimdata of a single trial from a Gazedata and Stimdata object. This function
#' is curried.
#'
#' @keywords internal
#' @param gazedata A Gazedata object.
#' @param stimdata A Stimdata object.
#' @param trial_number The trial number of the trial whose gazedata and stimdata
#'   will be combined.
#' @return A data.frame of the gazedata, augmented with attributes for the
#'   stimdata.
#' @usage CombineGazedataStimdata(gazedata, stimdata)(trial_number)
CombineGazedataStimdata <- function(gazedata, stimdata) {
  # A function that when applied to a trial number returns the combined
  # gazedata and stimdata (in the manner described above) for the trial.
  function(trial_number) {
    trial <- gazedata[gazedata$TrialNo == trial_number, ]
    target_image <- .GetTargetImage(stimdata, trial_number)

    trial <- within(trial, {
      # If the target image was on the left half of the screen, then the
      # X__ToTarget gazedata is the inverse of the corresponding X__ gazedata.
      if (is_image_on_left(target_image)) {
        XLeftToTarget  <- 1 - XLeft
        XRightToTarget <- 1 - XRight
        XMeanToTarget  <- 1 - XMean
      } else {
        XLeftToTarget  <- XLeft
        XRightToTarget <- XRight
        XMeanToTarget  <- XMean
      }
      # If the target image was on the lower half of the screen, then the
      # Y__ToTarget gazedata is the inverse of the corresponding Y__ gazedata.
      if (is_image_on_bottom(target_image)) {
        YLeftToTarget  <- 1 - YLeft
        YRightToTarget <- 1 - YRight
        YMeanToTarget  <- 1 - YMean
      } else {
        YLeftToTarget  <- YLeft
        YRightToTarget <- YRight
        YMeanToTarget  <- YMean
      }
    })
    # Add the stimdata attributes. Create Trial.
    attributes(trial) <- c(attributes(trial), .GetStimdata(stimdata, trial_number))
    as.Trial(trial)
  }
}

is_image_on_left <- function(image) grepl("Left", image) | image == "ImageL"
is_image_on_bottom <- function(image) grepl("Lower", image)


# Tell, don't ask.
.GetStimdata <- function(...) UseMethod(".GetStimdata")

.GetStimdata.Stimdata <- function(stimdata, trial_number) {
  stimdata[stimdata$TrialNo == trial_number, ]
}

.GetStimdata.default <- .GetStimdata.Stimdata

.GetTargetImage <- function(...) UseMethod(".GetTargetImage")

.GetTargetImage.Stimdata <- function(stimdata, trial_number) {
  .GetStimdata(stimdata, trial_number)$TargetImage
}

.GetTargetImage.default <- .GetTargetImage.Stimdata
