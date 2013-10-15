#' Combine Blocks into Sessions
#' 
#' Session is a generic function for initializing a Session object, which
#' represents the gazedata and stimdata for each trial in an experimental
#' session. The representation of each trial in a Session object is the same as
#' in a Block object. 
#' 
#' Session.list is a method for initializing a Session object
#' from a list of Block objects.
#' 
#' Session.character is a method for initializing a Session object from the path
#' of the directory that contains the .gazedata and .txt (stimdata) files for
#' both blocks of an experimental session.
#' 
#' @param list_of_blocks A list of Block objects, each Block object a block of the
#'   same experimental session. The Block objects in list_of_blocks should be
#'   ordered as they were presented during the experiment. That is, Block 1 of
#'   the experiment should be the first element of list_of_blocks, etc.
#' @param session_path The directory path of both pairs of .gazedata and .txt
#'   files for the session---one pair for each block.
#' @return A Session object.
#' @S3method Session list
#' @S3method Session character
#' @export
Session <- function(...) UseMethod('Session')

#' @method Session list
#' @rdname Session
Session.list <- function(list_of_blocks) {
  # Creating a Session object from a list of Block objects is very straight- 
  # forward, conceptually:  All that's really happening is that all the Blocks 
  # are concatenated together. The only real work is a small amount of 
  # book-keeping, making sure that the TrialNo of each block is incremented 
  # appropriately to reflect the number of that trial in the *session* rather 
  # than its number in the block in which it occurred.
  
  # First, get the number of trials in each block.
  trials_per_block <- Map(length, list_of_blocks)
  
  # Second, reduce trials_per_block to a list of the number of trials before
  # each block.
  trials_before_block <- Reduce(`+`, trials_per_block, init = 0, accumulate = TRUE)
  
  # Third, trials_before_block has 1 more element than it should. Get rid
  # of the last element.
  trials_before_block <- trials_before_block[1:length(list_of_blocks)]
  
  # Lastly, increment the the TrialNo of the Trials in each Block.
  updated_blocks <- Map(.IncrementTrialNosInBlock, list_of_blocks, trials_before_block)
  
  # Create a Session object by concatenating all of the blocks.
  session <- Reduce(c, updated_blocks)
  class(session) <- c('Session', 'list')  
  return(session)  
} 

#' @method Session character
#' @rdname Session
Session.character <- function(session_path) {
  # Get all the .gazedata files that are in the session directory.
  gazedata_files <- dir(session_path, pattern='gazedata', full.names=TRUE)
  
  if(length(gazedata_files) == 0){
    msg <- paste0("No `.gazedata` files found in ", getwd(), "/", session_path)
    stop(msg)
  }
  
  # Since the gazedata and stimdata files only differ in their file extensions,
  # strip off the .gazedata extensions to get the Block basenames
  block_paths <- tools::file_path_sans_ext(gazedata_files)
  
  # Initialize a Block object for each block path in block_paths, and call the
  # Session.list method.
  blocks <- Map(Block, block_paths)
  Session(blocks)
} 




#' Increment trial numbers for a whole block of Trials
#' 
#' .IncrementTrialNosInBlock is a utility function for incrementing by a fixed 
#' number, the trial number of each Trial object in a Block.
#' 
#' @param block A Block object
#' @param increment An integer that will be added to the TrialNo attribute of 
#'   each Trial in block.
#' @return A Block object that is identical to block, except that the TrialNo
#'   attributes of the Trials in it have been modified.
.IncrementTrialNosInBlock <- function(block, increment) {
  incremented_block <- Map(.IncrementTrialNo(increment), block)
  # Copy the class and attributes of block onto modified.block.
  class(incremented_block) <- class(block)
  attributes(incremented_block) <- attributes(block)
  return(incremented_block)
} 


#' Increment trial numbers
#' 
#' .IncrementTrialNo is a utility function for modifying the the TrialNo 
#' attribute of a trial. This function is curried.
#' 
#' @param trial A Trial object.
#' @param increment An integer that will be added to the current value of 
#'   TrialNo.
#' @return A Trial object that is identical to trial except that its TrialNo
#'   attribute has been modified.
#' @usage .IncrementTrialNo(increment)(trial)
.IncrementTrialNo <- function(increment) {
  LambdaTrial <- function(trial) {
    # Modify the TrialNo attribute of trial with the enclosed increment value
    trial %@% 'TrialNo' <- (trial %@% 'TrialNo') + increment
    return(trial)
  }
  return(LambdaTrial)
} 








#### Block initialization and methods -----------------------------------------

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
#' Block.Stimdata is a method for initializing a Block object from a Stimdata 
#' object and a Gazedata object. It's just Block.Gazedata with the gazedata and 
#' stimdata parameters reversed.
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
#'   
#' @S3method Block Gazedata
#' @S3method Block Stimdata
#' @S3method Block character
#' @export
Block <- function(...) UseMethod('Block')

#' @method Block Gazedata
#' @rdname Block
Block.Gazedata <- function(gazedata, stimdata) {
  # For each trial in the block, combine the gazedata and stimdata.
  trials_in_block <- Map(.CombineGazedataStimdata(gazedata, stimdata), stimdata$TrialNo)
  # Augment the class of trials.in.block.
  class(trials_in_block) <- c('Block', 'list')
  return(trials_in_block)
} 

#' @method Block Stimdata
#' @rdname Block
Block.Stimdata <- function(stimdata, gazedata) Block(gazedata, stimdata)

#' @method Block character
#' @rdname Block
Block.character <- function(block_path) {
  # Initialize the Gazedata object.
  gazedata_path <- sprintf('%s.gazedata', block_path)
  gazedata <- Gazedata(gazedata_path)
  # Initialize the Stimdata object.
  stimdata_path <- sprintf('%s.txt', block_path)
  stimdata <- Stimdata(stimdata_path)
  # Call Block.Gazedata.
  Block(gazedata, stimdata)
}




#' Combine Gazedata and Stimdata
#' 
#' .CombineGazedataStimdata is a utility function for combining the gazedata and
#' stimdata of a single trial from a Gazedata and Stimdata object. This function
#' is curried.
#' 
#' @param gazedata A Gazedata object.
#' @param stimdata A Stimdata object.
#' @param trial_number The trial number of the trial whose gazedata and stimdata
#'   will be combined.
#' @return A data.frame of the gazedata, augmented with attributes for the 
#'   stimdata.
#' @usage .CombineGazedataStimdata(gazedata, stimdata)(trial_number)
.CombineGazedataStimdata <- function(gazedata, stimdata) {
  # A function that when applied to a trial number returns the combined
  # gazedata and stimdata (in the manner described above) for the trial.
  .LambdaTrialNo <- function(trial_number) {
    trial_gazedata <- subset(gazedata, TrialNo == trial_number)
    target_image <- stimdata[stimdata$TrialNo == trial_number, 'TargetImage']
    
    # Add the ___ToTarget gazedata according to the location of the target image.
    IsImageOnLeft <- function(image) grepl("Left", image) | image == "ImageL"
    IsImageOnBottom <- function(image) grepl("Lower", image)
    
    # If the target image was on the left half of the screen, then the
    # X__ToTarget gazedata is the inverse of the corresponding X__ gazedata.
    trial_gazedata <- within(trial_gazedata, {
      if (IsImageOnLeft(target_image)) {
        XLeftToTarget  <- 1 - XLeft
        XRightToTarget <- 1 - XRight
        XMeanToTarget  <- 1 - XMean
      } else {
        XLeftToTarget  <- XLeft
        XRightToTarget <- XRight
        XMeanToTarget  <- XMean  
      }
    })
    
    # If the target image was on the lower half of the screen, then the 
    # Y__ToTarget gazedata is the inverse of the corresponding Y__ gazedata.
    
    trial_gazedata <- within(trial_gazedata, {
      if (IsImageOnBottom(target_image)) {
        YLeftToTarget  <- 1 - YLeft
        YRightToTarget <- 1 - YRight
        YMeanToTarget  <- 1 - YMean
      } else {
        YLeftToTarget  <- YLeft
        YRightToTarget <- YRight
        YMeanToTarget  <- YMean
      }
    })
    
    # Add the stimdata attributes. Augment the class of trial_gazedata.
    attributes(trial_gazedata) <- c(attributes(trial_gazedata),
                                    stimdata[stimdata$TrialNo == trial_number, ])
    class(trial_gazedata) <- c('Trial', 'data.frame')
    return(trial_gazedata)
  }
}
