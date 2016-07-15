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
  class(trial_list) <- "list"
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



#' lookr_classes
#' @name lookr_classes
NULL

#' @rdname lookr_classes
#' @param x an object to coerce to another class (as.Class function) or an
#'   object whose class we are checking (is.Class) function
#' @export
as.Task <- function(x) {
  class(x) <- c("Task", "TrialList", "list")
  x
}

#' @rdname lookr_classes
#' @export
as.Session <- function(x) {
  class(x) <- c("Session", "TrialList", "list")
  x
}

#' @rdname lookr_classes
#' @export
as.Block <- function(x) {
  class(x) <- c("Block", "TrialList", "list")
  x
}

#' @rdname lookr_classes
#' @export
as.TrialList <- function(x) {
  class(x) <- c("TrialList", "list")
  x
}

#' @rdname lookr_classes
#' @export
as.Trial <- function(x) {
  class(x) <- c("Trial", "data.frame")
  x
}

#' @rdname lookr_classes
#' @export
as.Gazedata <- function(x) {
  class(x) <- c("Gazedata", "data.frame")
  x
}


#' @rdname lookr_classes
#' @export
is.Task <- function(x) inherits(x, "Task")

#' @rdname lookr_classes
#' @export
is.Session <- function(x) inherits(x, "Session")

#' @rdname lookr_classes
#' @export
is.Block <- function(x) inherits(x, "Block")

#' @rdname lookr_classes
#' @export
is.TrialList <- function(x) inherits(x, "TrialList")

#' @rdname lookr_classes
#' @export
is.Trial <- function(x) inherits(x, "Trial")


#' @export
print.Trial <- function(x, ...) str(x, ...)

#' @export
print.Gazedata <- function(x, ...) str(x, ...)

#' @export
print.Stimdata <- function(x, ...) str(x, ...)

#' @export
print.TrialList <- function(x, ...) {
  # Count trials
  trials <- x
  d <- gather_attributes(trials, c("Subject", "Basename", "TrialNo"))
  trial_summary <- count(d, c("Subject", "Basename"))
  names(trial_summary) <- c("SubjectID", "Basename", "Trials")

  # Prepare header
  n_subjects <- length(unique(trial_summary$SubjectID))
  n_trials <- sum(trial_summary$Trials)
  header <- sprintf("%s object with %s Subject IDs and %s trials: \n",
                    class(trials)[1], n_subjects, n_trials)

  cat(header)
  print(trial_summary)
}
