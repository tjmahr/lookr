
#' Compute empirical logit
#'
#' @param x vector containing number of looks to target
#' @param y vector containing number of looks to distractors
#' @return \code{empirical_logit} returns the empirical logit of looking to
#'   target. \code{empirical_logit_weight} returns weights for these values.
#' @export
#' @references Dale Barr's
#' \href{http://talklab.psy.gla.ac.uk/tvw/elogit-wt.html}{Walkthrough of an
#' "empirical logit" analysis in R}
empirical_logit <- function(x, y) {
  log((x + 0.5) / (y + 0.5))
}

#' @rdname empirical_logit
#' @export
empirical_logit_weight <- function(x, y) {
  var1 <- 1 / (x + 0.5)
  var2 <- 1 / (y + 0.5)
  var1 + var2
}

#' Compute orthogonal times
#'
#' @param times a vector of time values
#' @param degree of the desired polynomial
#' @return a data-frame with original time values and an \code{ot} column for
#'   each polynomial degree
#' @export
orthogonal_time <- function(times, degree) {
  clean_times <- sort(unique(times))
  time_df <- as.data.frame(poly(clean_times, degree))
  names(time_df) <- paste0("ot", names(time_df))
  time_df$Time <- clean_times
  time_df
}

frame_time <- function(time, bins = 3) {
  (time / lwl_constants$ms_per_frame) / bins
}

# inverse_logit <- gtools::inv.logit

