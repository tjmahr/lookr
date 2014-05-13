
empirical_logit <- function(x, y) {
  log((x + 0.5) / (y + 0.5))
}

empirical_logit_weight <- function(x, y) {
  var1 <- 1 / (x + 0.5)
  var2 <- 1 / (y + 0.5)
  var1 + var2
}
