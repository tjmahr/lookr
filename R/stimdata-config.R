#' Make a stimdata configuration
#' 
#' \code{MakeStimdataConfig} specifies how \code{ExtractStim} processes an
#' Eprime stimdata file.
#' 
#' @param named_stim a character vector of named values that specifies which 
#'   stimdata fields to extract from an Eprime log file.
#' @param numerics names of stimuli in \code{named_stim} that represent numeric 
#'   values.
#' @param constants constant values to specify after the \code{named_stim} have 
#'   been extracted.
#' @param derived a character vector of expressions to execute once constants 
#'   have been added to the stimdata description.
#' @return a list with the class \code{StimConfig}. This list has four slots: 
#'   \code{Stim}, \code{Numerics}, \code{Constants}, and \code{Derived}.
#' @export
MakeStimdataConfig <- function(named_stim, numerics = NULL, constants = NULL, derived = NULL) {
  structure(list(
    Stim = named_stim,
    Numerics = numerics,
    Constants = constants,
    Derived = derived), 
            class = c("StimConfig", "list"))
}




#' Modify a \code{StimConfig} list
#' 
#' @param config a \code{StimConfig} list
#' @param level the part of the list to modify: \code{Stim}, \code{Numerics}, 
#'   \code{Constants}, or \code{Derived}
#' @param value the value(s) to append to the StimConfig at the given level.
#' @return an updated copy of \code{StimConfig}
#' 
#' @details
#' Shortcut functions are provided by \code{AddStim}, \code{AddNumerics}, 
#' \code{AddConstants}, \code{AddNumericConstant}, and \code{AddDerived}.
#'   
#' Named stimuli need to be wrapped inside of \code{c()}. For example, to add
#' a Dialect constant of "SAE" to \code{config}, use \code{AddConstant(config,
#' c(Dialect = "SAE"))}. This prevents the function from interpreting
#' "\code{Dialect = dialect}" as a function parameter.
#' @export
UpdateConfig <- function(config, level, value) {
  values <- config[[level]]
  config[[level]] <- c(values, value)
  config
}

#' @rdname UpdateConfig
AddStim <- function(config, value) UpdateConfig(config, "Stim", value)

#' @rdname UpdateConfig
AddNumerics <- function(config, value) UpdateConfig(config, "Numerics", value)

#' @rdname UpdateConfig
AddConstants <- function(config, value) UpdateConfig(config, "Constants", value)

#' @rdname UpdateConfig
AddDerived <- function(config, value) UpdateConfig(config, "Derived", value)

#' @rdname UpdateConfig
AddNumericConstant <- function(config, value) {
  config <- AddNumerics(config, names(value))
  config <- AddConstants(config, value)
  config
}

