#' Get or set attributes of an object
#'
#' \code{\%@@\%} is an infix-form wrapper for the \code{attr} function.
#'
#' @details
#' A \code{Trial} object is a data-frame of looking data, and metadata about the
#' trial like the images onscreen or duration of the carrier phrase are stored
#' in attributes attached to the data-frame. The functions described below
#' making working with attributes a little easier.
#'
#' \code{\%@@\%} is a utility function for getting the value of an attribute
#' from an R object. This is a wrapper for the built-in function \code{attr}.
#' \code{\%@@\%.list} applies \code{\%@@\%} onto each object in a list of R
#' objects. Therefore, \code{\%@@\%} cannot grab attributes of lists, unless
#' they are lists within lists. This is weird; maybe I should fix this.
#'
#' Attributes can be set two ways:
#'
#' \enumerate{
#'  \item \code{x <- SetAttribute(x, attribute, value)}
#'  \item \code{x \%@@\% attribute <- value}
#' }
#'
#' If the object is a \code{TrialList} then then assignment is vectorized,
#' meaning that each object (i.e., \code{Trial}) is updated with the
#' \code{value} (which is recycled as needed).
#'
#' @param x An object with attributes
#' @param attribute A character string that names an attribute of the object.
#' @param value a new value for an attribute
#' @return The value in the attribute slot of object, or the values of the
#'   attribute for each element element in a list.
#'
#' @examples
#' Cat <- function(name, owner) {
#'  structure(list(Name = name), Owner = owner, class = "cat")
#' }
#'
#' kiki <- Cat("Kiki", "AML")
#' nooper <- Cat("Nooper", "TJM")
#'
#' kiki %@@% "Owner"
#' # [1] "AML"
#'
#' list(nooper, kiki) %@@% "Owner"
#' # [1] "TJM" "AML"
#'
#' # Setting/overwriting a attribute
#' nooper %@@% "Owner" <- NA
#' list(nooper, kiki) %@@% "Owner"
#' # [1] NA    "AML"
#' @name attributes
#' @rdname attributes
#' @export
`%@%` <- function(x, attribute) UseMethod('%@%')

#' @export
`%@%.default` <- function(x, attribute) attr(x, attribute, exact = TRUE)

#' @export
`%@%.list` <- function(x, attribute, exact = TRUE) {
  sapply(x, function(y) attr(y, attribute))
}


#' @rdname attributes
#' @export
SetAttribute <- function(x, attribute, value) UseMethod("SetAttribute")

#' @export
SetAttribute.default <- function(x, attribute, value) {
  `attr<-`(x, attribute, value)
}

#' @export
SetAttribute.TrialList <- function(x, attribute, value) {
  trials <- x
  classes <- class(trials)
  trials <- Map(SetAttribute, trials, attribute = attribute, value = value)
  class(trials) <- classes
  trials
}

#' @rdname attributes
#' @usage x \%@@\% attribute <- value
#' @export
`%@%<-` <- function(x, attribute, value) {
  UseMethod("SetAttribute")
}



#' Make a function to filter out trials based on attribute values
#'
#' This is a higher-order function.
#'
#' @param attr_name the name of attribute to filter by
#' @param trials a list of Trial objects
#' @param attr_value a vector of values to filter out
#' @return a function \code{f(trials, attr_values)} that filters out trials with
#'   the specified \code{attr_values} for the given \code{attr_name}.
#' @export
MakeAttributeFilter <- function(attr_name) {
  function(trials, attr_value) {
    classes <- class(trials)
    trials <- trials[!is.element(trials %@% attr_name, attr_value)]
    class(trials) <- classes
    trials
  }
}
