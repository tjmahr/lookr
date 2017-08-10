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
#' @param blocks A list of Block objects, each Block object a block of
#'   the same experimental session. The Block objects in list_of_blocks should
#'   be ordered as they were presented during the experiment. That is, Block 1
#'   of the experiment should be the first element of list_of_blocks, etc.
#' @param ... Additional arguments passed onto S3 methods. Currently ignored.
#' @return A Session object.
#' @export
Session <- function(...) UseMethod("Session")

#' @export
#' @rdname Session
Session.character <- function(session_path, ...) {
  # Get all the .gazedata files that are in the session directory.
  gazedata_files <- dir(session_path, pattern = "gazedata", full.names = TRUE)
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
#' @rdname Session
Session.list <- function(blocks, ...) {
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
#' @param ... Additional arguments passed onto S3 methods. Currently ignored.
#' @return A Block object---i.e., a list of trials, where each trial is
#'   represented as a data.frame of its gazedata, augmented with attributes for
#'   the stimdata.
#' @export
Block <- function(...) UseMethod("Block")

#' @export
#' @rdname Block
Block.character <- function(block_path, ...) {
  gazedata <- Gazedata(paste0(block_path, ".gazedata"))
  stimdata <- Stimdata(paste0(block_path, ".txt"))
  Block(gazedata, stimdata)
}

#' @export
#' @rdname Block
Block.Gazedata <- function(gazedata, stimdata, ...) {
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
#' @return A data.frame of the gazedata, augmented with attributes for the
#'   stimdata.
#' @details The function returns a higher order function with the
#'   \code{gazedata} and \code{stimdata} fixed. Details about the returned
#'   function:
#' \describe{
#'   \item{Usage}{
#'   \code{CombineGazedataStimdata(gazedata, stimdata)(trial_number)}}
#'   \item{\code{trial_number}}{The trial number of the trial whose gazedata
#'   and stimdata will be combined.}
#' }
CombineGazedataStimdata <- function(gazedata, stimdata) {
  # A function that when applied to a trial number returns the combined
  # gazedata and stimdata (in the manner described above) for the trial.
  function(trial_number) {
    trial <- gazedata[gazedata$TrialNo == trial_number, ]
    # Add the stimdata attributes. Create Trial.
    attributes(trial) <- c(attributes(trial), .GetStimdata(stimdata, trial_number))
    as.Trial(trial)
  }
}

# Tell, don't ask.
.GetStimdata <- function(...) UseMethod(".GetStimdata")

.GetStimdata.Stimdata <- function(stimdata, trial_number) {
  stimdata[stimdata$TrialNo == trial_number, ]
}

.GetStimdata.default <- .GetStimdata.Stimdata
