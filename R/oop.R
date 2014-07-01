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
c.Session <- function(session, ...) {
  # Coerce session to a list and then call the c.list method on session and ...
  class(session) <- 'list'
  trials <- c(session, ...)

  # If there are multiple subjects, return a "Task".
  if (unique_task(trials)) {
    if (unique_subject(trials)) as.Session(trials) else as.Task(trials)
  }
  # WE may need "else trials" if we ever want to combine trials with different
  # tasks.
}

# The checks inside c.Session should work for combining Task objects.
#' @export
c.Task <- function(task, ...) c.Session(task, ...)

#' @export
as.Session <- function(x) {
  if (!is.Session(x)) class(x) <- c("Session", "list")
  x
}

#' @export
as.Task <- function(x) {
  if (!is.Task(x)) class(x) <- c("Task", "list")
  x
}

#' @export
is.Session <- function(x) inherits(x, "Session")

#' @export
is.Task <- function(x) inherits(x, "Task")

#' @export
print.Trial <- function(trial, ...) str(trial, ...)

#' @export
print.Gazedata <- function(...) str(...)

#' @export
print.Stimdata <- function(...) str(...)


# Check whether trials should be a Task or Session object.
unique_attr_checker <- function(attr_name) {
  function(trials) n_distinct(trials %@% attr_name) == 1
}
unique_task <- unique_attr_checker("Task")
unique_subject <- unique_attr_checker("Subject")

n_distinct <- function(xs) length(unique(xs))
