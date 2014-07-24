## S3 methods and other functions that care about classes of objects




#' Apply a function to a list of trials, preserving classes of the list
#'
#' This function is just a wrapper for \code{lapply} that makes sure that the
#' classes of the inputted list are preserved in the outputted list.
#'
#' @param trials a list of Trial objects
#' @param trial_func a function to apply on each Trial object
#' @param ... additional arguments passed to the function
#' @export
trial_lapply <- function(trials, trial_func, ...) {
  classes <- class(trials)
  trials <- lapply(trials, trial_func, ...)
  class(trials) <- classes
  trials
}


# Combine lists of Trials.
#' @export
c.TrialList <- function(trial_list, ...) {
  # Coerce trial_list to a list, so it doesn't infinitely recurse
  class(trial_list) <- 'list'
  trials <- c(trial_list, ...)

  # Single subject, single task: Session
  # Multi subjects, single task: Task
  # Otherwise: TrialList
  if (!unique_task(trials)) {
    as.TrialList(trials)
  } else if (unique_subject(trials)) {
    as.Session(trials)
  } else {
    as.Task(trials)
  }
}


# Check whether trials should be a Task or Session object.
unique_attr_checker <- function(attr_name) {
  function(trials) n_distinct(trials %@% attr_name) == 1
}
unique_task <- unique_attr_checker("Task")
unique_subject <- unique_attr_checker("Subject")
n_distinct <- function(xs) length(unique(xs))

#' @export
as.Task <- function(x) {
  if (!is.Task(x)) class(x) <- c("Task", "TrialList", "list")
  x
}

#' @export
as.Session <- function(x) {
  if (!is.Session(x)) class(x) <- c("Session", "TrialList", "list")
  x
}

#' @export
as.Block <- function(x) {
  if (!is.Block(x)) class(x) <- c("Block", "TrialList", "list")
  x
}

#' @export
as.TrialList <- function(x) {
  if (!is.TrialList(x)) class(x) <- c("TrialList", "list")
  x
}


#' @export
is.Task <- function(x) inherits(x, "Task")

#' @export
is.Session <- function(x) inherits(x, "Session")

#' @export
is.Block <- function(x) inherits(x, "Block")

#' @export
is.TrialList <- function(x) inherits(x, "TrialList")

#' @export
print.Trial <- function(trial, ...) str(trial, ...)

#' @export
print.Gazedata <- function(...) str(...)

#' @export
print.Stimdata <- function(...) str(...)

