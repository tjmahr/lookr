`%contains%` <- function(x, y) any(y %in% x)

`%lacks%` <- function(x, y) !any(y %in% x)

is_all_na <- function(x) all(is.na(x))

#' Divide a vector into equal-sized chunks, except for maybe the last one
#'
#' @param xs a vector
#' @param step the length of a chunk
#' @return a list with the elements of `xs` partitioned into `step`-sized chunks
slice <- function(xs, step = 5) {
  last_tail <- length(xs)
  heads <- seq(1, last_tail, step)
  expand_head <- function(this_head) {
    next_tail <- this_head + (step - 1)
    this_tail <- min(next_tail, last_tail)
    xs[seq(this_head, this_tail)]
  }
  lapply(heads, expand_head)
}


#' Use the values in one column to extract values from other columns.
#'
#' @param df a data-frame
#' @param col_name a string naming a column whose values are the names of the
#'   other columns in the data-frame.
#' @return a vector of values from the columns named in df$col_name
#' @examples
#' # The Location column names other columns
#' df <- data.frame(stringsAsFactors = FALSE,
#'   Location = c("Left", "Right", "Right", "Left"),
#'   Left = c("l1", "l2", "l3", "l4"),
#'   Right = c("r1", "r2", "r3", "r4"))
#' teleport_by_column(df, "Location")
#' # [1] "l1" "r2" "r3" "l4"
#' # We use the values in "Location" to teleport into those other columns
teleport_by_column <- function(df, col_name) {
  rows <- seq_len(nrow(df))
  # Are the values in the pathway-column also column names?
  values <- df[[col_name]]
  stopifnot(all(values %in% colnames(df)), length(col_name) == 1)
  # Curry the `x[i,j]` function so we can fix the x parameter and map over the
  # is, js
  extract_ij <- function(x) function(i, j) x[i, j]
  hits <- unlist(Map(extract_ij(df), i = rows, j = values))
  hits
}
