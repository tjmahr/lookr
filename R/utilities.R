`%contains%` <- function(x, y) any(y %in% x)

`%lacks%` <- function(x, y) !any(y %in% x)

is_all_na <- function(x) all(is.na(x))
