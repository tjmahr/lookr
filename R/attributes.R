#' Get or set attributes of an object (vectorized)
#'
#' \code{\%@@\%} is an infix-form wrapper for the \code{attr} function.
#'
#' @details A \code{Trial} object is a data-frame of looking data, and metadata
#' about the trial like the images onscreen or duration of the carrier phrase
#' are stored in attributes attached to the data-frame. The functions described
#' below making working with attributes a little easier.
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
#'     \item \code{x <- SetAttribute(x, attribute, value)}
#'     \item \code{x \%@@\% attribute <- value}}
#'
#' If the object is a \code{TrialList} then then assignment is vectorized,
#' meaning that each object (i.e., \code{Trial}) is updated with the
#' \code{value} (which is recycled as needed).
#'
#' \code{\%try@@\%} replaces \code{NULL} values with \code{NA} if the attribute is
#' missing.
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
`%@%.list` <- function(x, attribute) {
  sapply(x, function(y) attr(y, attribute, exact = TRUE))
}

#' @export
#' @describeIn attributes
`%try@%` <- function(x, attribute) {
  values <- x %@% attribute
  # If just NULL, one NA
  if (length(values) == 0) values <- NA
  # If many NULLS, many NAs
  values <- ifelse(sapply(values, is.null), NA, values)
  values
}


#' @rdname attributes
#' @export
SetAttribute <- function(x, attribute, value) {
  UseMethod("SetAttribute")
}

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


#' Create a data-frame of attributes
#'
#' This function is intended to abstract over the pattern of writing code like
#' \code{data.frame(x = trials \%@@\% "x", ZName = trials \%@@\% "z")} by
#' instead allowing us to write \code{gather_attributes(trials, c("x", ZName =
#' z))}.
#'
#' @param x a list of objects with attributes
#' @param attrs a character vector. If an element in the vector is named, the
#'   name is used as the new (column) name for that attribute's column in the
#'   resulting data-frame.
#' @param omit_na whether to drop columns with all \code{NA} values
#' @return a data-frame with a column for each of the attributes in
#'   \code{attrs}. By default, non-existent attributes will be stored with NA
#'   values and strings are not factors.
#' @export
gather_attributes <- function(x, attrs, omit_na = FALSE) {
  # Set names of unnamed attributes
  if (is.null(names(attrs))) names(attrs) <- attrs
  names(attrs) <- ifelse(names(attrs) == "", attrs, names(attrs))

  # Get each attribute
  get_this_attr <- function(this_attr) x %try@% this_attr
  attr_list <- lapply(attrs, get_this_attr)

  # List names are inherited from the names of the attrs
  stopifnot(names(attr_list) == names(attrs))
  attr_table <- as.data.frame(attr_list, stringsAsFactors = FALSE)

  if (omit_na) {
    # Which columns have all NA values
    all_nas <- unlist(colwise(is_all_na)(attr_table))
    to_drop <- names(attr_table)[all_nas]

    # Null them
    attr_table[, to_drop] <- list(NULL)
  }

  attr_table
}
