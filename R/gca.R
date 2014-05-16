
empirical_logit <- function(x, y) {
  log((x + 0.5) / (y + 0.5))
}

empirical_logit_weight <- function(x, y) {
  var1 <- 1 / (x + 0.5)
  var2 <- 1 / (y + 0.5)
  var1 + var2
}

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

