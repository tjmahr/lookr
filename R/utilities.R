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
