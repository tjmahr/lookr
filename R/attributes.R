#' Get or set attributes of an object
#' 
#' \code{\%@@\%} is an infix-form wrapper for the \code{attr} function.
#' 
#' @details
#' 
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
#' Attributes of single objects can be set can be set using \code{object \%@@\% 
#' attribute <- value}.
#' 
#' @param x An object with attributes
#' @param xs A list of objects (with attributes).
#' @param attribute A character string that names an attribute of the object.
#' @param value a new value for an attribute
#' @return The value in the attribute slot of object, or the values of the 
#'   attribute for each element element in a list.
#'   
#' @name attributes
#' @rdname attributes
#' @export
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
`%@%` <- function(x, attribute) UseMethod('%@%')

#' @rdname attributes
`%@%.default` <- function(x, attribute) attr(x, attribute)

#' @rdname attributes
`%@%.list` <- function(xs, attribute) {
  sapply(xs, function(x) attr(x, attribute))
}

#' @rdname attributes
#' @usage x \%@@\% attribute <- value
#' @export 
`%@%<-` <- function(x, attribute, value) { 
  `attr<-`(x, attribute, value)
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
MakeAttributeFilter <- function(attr_name) {
  function(trials, attr_value) {
    classes <- class(trials)
    trials <- trials[!is.element(trials %@% attr_name, attr_value)] 
    class(trials) <- classes
    trials
  }
}
