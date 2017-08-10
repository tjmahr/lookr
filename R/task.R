#' Load the sessions for an experimental task
#'
#' \code{Block(...)} collects the trials for a single looking-while-listening
#' experiment. \code{Session(...)} combines blocks for a single subject.
#' \code{Task(...)} combines sessions over multiple subjects.
#'
#' A \code{Task} object is a list of \code{Trial} objects, which all have the
#' same \code{"Task"} attribute but do not have the same \code{"Subject"}
#' attribute.
#'
#' @param task_path a task-level directory. The subdirectories of
#'   \code{task_path} should be subject directories, that begin with the pattern
#'   \code{[0-9]{3}[CLPX]}, and these subject directories should contain
#'   experimental blocks.
#' @param partial a vector of integers used to select some of the
#'   subdirectories. Used to partially load the set of sessions for the task, in
#'   order to prototype code or test the function. Default is \code{NA}.
#' @param handler a function used to handle error messages. By default, an error
#'   message is caught and printed as a \code{warning}.
#' @param ... Additional arguments passed onto S3 methods. Currently ignored.
#' @return a Task object containing all the trials from all the sessions that
#'   could be successfully loaded. If a block could not be loaded, its error
#'   message is handled using the specified handle function.
#' @export
Task <- function(...) UseMethod("Task")

#' @export
#' @rdname Task
Task.character <-  function(task_path, partial = NA, handler = warning, ...) {
  # Find subject subdirectories
  paths <- ListSubjectsInTaskDir(task_path)
  if (!all(is.na(partial))) {
    paths <- paths[intersect(partial, seq_along(paths))]
  }
  # Try to load the sessions, filter out the failed sessions
  sessions <- lapply(paths, function(x) TryLoad(x, handler))
  sessions <- Filter(function(x) !is.null(x), sessions)
  do.call(c, sessions)
}




#' List all subdirectories that begin with an L2T identifier
#'
#' This is the first step in gathering all the data in an LWL experimental task.
#'
#' @param gaze_dir the path to a directory containing subject subdirectories
#' @return a character vector of the subdirectories that begins with three
#'   digits followed by "C", "L", "P" or "X" --- i.e., paths that begin
#'   with a L2T subject identifier.
ListSubjectsInTaskDir <- function(gaze_dir) {
  l2t_dir <- lwl_constants$l2t_subject_dir
  subject_paths <- list.files(gaze_dir, pattern = l2t_dir, full.names = TRUE)
  # Keep only directories
  subject_paths[file.info(subject_paths)$isdir]
}




#' Try to load an experimental block, failing gracefully if unsuccessful
#'
#' \code{TryLoad(...)} tries to load the blocks in a subject directory. If it
#' encounters an error, it catches the error and moves on to the next in the
#' subject directory.
#'
#' @param subject_path a path to a subject's directory which may contain blocks
#'   from LWL tasks.
#' @param handler a function to apply to the file-path of an experimental block
#'   hat raised an error. The default is to \code{print} the file-path.
#' @return \code{NULL} if there are no loadable experimental blocks in
#'   \code{subject_path}, otherwise a \code{Session} containing the blocks that
#'   could be loaded from \code{subject_path}.
#' @export
TryLoad <- function(subject_path, handler = warning) {
  gaze_files <- dir(subject_path, pattern = "gazedata", full.names = TRUE)
  # Attempt to load the blocks
  block_names <- file_path_sans_ext(gaze_files)
  blocks <- lapply(block_names, function(path) try(Block(path)))
  # Look for errors. Record them, then exclude them.
  failures <- vapply(blocks, is.error, TRUE)
  bad_blocks <- block_names[failures]
  blocks <- blocks[!failures]
  # SIDE-EFFECTING: Report errors
  for (block in bad_blocks) handler(block)
  # Return a session only if there is a usable block
  if (!is.empty(blocks)) Session(blocks) else NULL
}


is.empty <- function(x) length(x) == 0
is.error <- function(x) inherits(x, "try-error")
