#' Make an options list
#' 
#' This function was copied from 
#' [`knitr:::new_defaults`](https://github.com/yihui/knitr/blob/master/R/defaults.R).
#' It encloses the default values inside of a list's environment and uses 
#' functions stored in that list to manipulate and retrieve those defaults.
#' 
#' @param value a list of options and their respective values
#' @return list of four functions: `$get(name, default = FALSE)` to get the 
#'   value of an option name, `$set(...)` for setting option values, 
#'   `$merge(values)` for internally merging lists of options, `$restore(...)` 
#'   for restoring the original default values.
#'   
#' @examples
#' lwl_opts <- .MakeNewDefaults(list(
#'   interpolation_window = 100, 
#'   timeslice_start = -200, 
#'   timeslice_end = 2000
#' ))
#' 
#' lwl_opts$get()
#' # $interpolation_window
#' # [1] 100
#' # 
#' # $timeslice_start
#' # [1] -200
#' # 
#' # $timeslice_end
#' # [1] 2000
#' 
#' # Set new values
#' lwl_opts$set(interpolation_window = 200, timeslice_start = 250)
#' lwl_opts$get("interpolation_window")
#' # [1] 200
#' 
#' # Restore defaluts
#' lwl_opts$restore()
#' lwl_opts$get()
#' # $interpolation_window
#' # [1] 100
#' # 
#' # $timeslice_start
#' # [1] -200
#' # 
#' # $timeslice_end
#' # [1] 2000
.MakeNewDefaults <- function(value = list()) {
  # Hang onto the default values passed into this function
  defaults <- value
  
  get <- function(name, default = FALSE) {
    # Get original `defaults` value. This `defaults` is only a local version
    # (local inside this `get` function).
    if (default) defaults <- value
    # Handle zero names by giving everthing. Otherwise, handle one name and
    # multiple names appropriately.
    if (missing(name)) defaults else {
      if (length(name) == 1) defaults[[name]] else defaults[name]
    }
  }
  
  set <- function(...) {
    dots <- list(...)
    if (length(dots) == 0) return()
    # Handle when `...` is a `list()`
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]]))
      dots <- dots[[1]]
    defaults <<- merge(dots)
    invisible(NULL)
  }

  merge <- function(values) MergeLists(defaults, values)
  restore <- function(target = value) defaults <<- target
    
  list(get = get, set = set, merge = merge, restore = restore)
}

MergeLists <- function(x, y) {
  x[names(y)] <- y
  x
}
