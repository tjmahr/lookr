#' Make an Option List
#' 
#' This function was copied from 
#' \href{https://github.com/yihui/knitr/blob/master/R/defaults.R}{\code{knitr:::new_defaults}}.
#' It encloses the default values inside of a list's environment and uses 
#' functions stored in that list to manipulate and retrieve those defaults.
#' 
#' @export
#' @param value a list of options and their respective values
#' @return an \code{OptionList} containing four functions: \code{$get(name,
#'   default = FALSE)} to get the value of an option name, \code{$set(...)} for
#'   setting option values, \code{$merge(values)} for internally merging lists
#'   of options, \code{$restore(...)} for restoring the original default values.
#'   
#' @examples
#' lwl_opts <- OptionList(list(
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
#' # Get the default window value
#' lwl_opts$get("interpolation_window", default = TRUE)
#' # $interpolation_window
#' # [1] 100
#' 
#' # Restore defaluts
#' lwl_opts$restore()
#' 
#' # The `print(...)` method for an `OptionList` just prints `$get(...)`
#' lwl_opts
#' # $interpolation_window
#' # [1] 100
#' # 
#' # $timeslice_start
#' # [1] -200
#' # 
#' # $timeslice_end
#' # [1] 2000
OptionList <- function(value = list()) {
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
  
  merge <- function(values) merge_lists(defaults, values)
  restore <- function(target = value) defaults <<- target
  
  structure(list(get = get, set = set, merge = merge, restore = restore), 
            class = "OptionList")
}


merge_lists <- function(x, y) {
  x[names(y)] <- y
  x
}


#' @export
lwl_opts <- OptionList(list(
  interpolation_window = 150, 
  timeslice_start = -200, 
  timeslice_end = 2000
))


#' @export
print.OptionList <- function(option_list, ...) print(option_list$get(...))
