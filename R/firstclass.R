#' Create a function that grabs all trials with a certain attribute
#' 
#' @param attr_name a string naming the attribute by which to select trials
#' @param reversed a boolean that specifies whether the parameters in the 
#'   returns function should be reversed. Default value is `FALSE`, so the 
#'   function will have the argument order `function(trials, attr_value)`. 
#'   This is useful for `apply`-like functions.
#' @return A two-argument function that will grab all `Trial` objects (`trials`)
#'   with a certain attribute value (`attr_value`)
#'   
#' @examples
#' 
#' # Define a function that selects trials based on the StimType attribute
#' GetTrialsByStimType <- MakeTrialGrabber("StimType")
#' # Grab all trials with a StimType value of "neutral"
#' neutrals <- GetTrialsByStimType(trials, "neutral")
#' 
#' # Make function to select trials based on TargetWord. Grab all "duck" trials.
#' GetTrialsByTarget <- MakeTrialGrabber("TargetWord")
#' duck_trials <- GetTrialsByTarget(trials, "duck")
MakeTrialGrabber <- function(attr_name, reversed = FALSE) {
  if (reversed) {
    function(attr_value, trials) {
      classes <- class(trials)
      subset_of_trials <- trials[trials %@% attr_name == attr_value]
      class(subset_of_trials) <- classes
      subset_of_trials
    } 
  } else {
    function(trials, attr_value) {
      classes <- class(trials)
      subset_of_trials <- trials[trials %@% attr_name == attr_value]
      class(subset_of_trials) <- classes
      subset_of_trials
    }  
  }
}


#' Drop trials from a Session
#' 
#' @param trials a `list` of `Trial` objects
#' @param drop an integer vector specifying which trials should be dropped
#' @return the `list` of `Trial` objects minus the dropped trials. The classes
#'   of the input list are preserved in the outputted list.
DropTrials <- function(trials, drop) {
  if (length(drop) == 0) return(trials)  
  classes <- class(trials)
  trials <- trials[-drop]
  class(trials) <- classes
  return(trials)
}


