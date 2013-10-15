#' Manipulate attributes of objects
#' 
#' `%@@%` is a utility function for getting the value of an attribute from an R 
#' object. This is a wrapper for the built-in function `attr`.
#' 
#' `%@@%.list` applies `%@@%` onto each object in a list of R objects. Therefore, 
#' `%@@%` cannot grab attributes of lists, unless they are lists within lists. 
#' This is weird; maybe I should fix this.
#' 
#' @usage `%@@%` should be written as infix (like a division sign)
#'   
#' @param object An R object with attributes
#' @param attribute A character string that names an attribute of the object.
#' @return The value in the attribute slot of object, or the values of the 
#'   attribute for each element element in a list.
#'   
#' @S3method '%@@%' default
#' @S3method '%@@%' list
#' @export
`%@%` <- function(object, attribute) UseMethod('%@%')

#' @method '%@@%' default
#' @rdname '%@@%'
`%@%.default` <- function(object, attribute) attr(object, attribute)

#' @method '%@@%' list
#' @rdname '%@@%'
#' @param xs A list of objects (with attributes).
#' @examples
#' MakeCat <- function(name, owner) { 
#'  structure(list(Name = name), Owner = owner, class = "cat")
#' }
#' kiki <- MakeCat("Kiki", "AML")
#' nooper <- MakeCat("Nooper", "TJM")
#' kiki %@% "Owner"
#' # [1] "AML"
#' list(nooper, kiki) %@% "Owner"
#' # [1] "TJM" "AML"
#' nooper %@% "Owner" <- NA
#' list(nooper, kiki) %@% "Owner"
#' # [1] NA    "AML"
`%@%.list` <- function(xs, attribute) {
  sapply(xs, function(x) attr(x, attribute))
}




# `%@@%<-` is a utility function for setting the value of an attribute of an
# object.  This is just syntactic sugar for the built-in function `attr<-`.
# The syntax of %@@%<- isn't obvious, but it should be written as infix:
#   R> x <- 0
#   R> x %@% 'test' <- 'hello'  # this is equivalent to:
#                               #   R> attr(x, 'test') <- 'hello'
#   R> x %@% 'test'
#      [1] 'hello'
# Arguments:
#      object: Any R object.
#   attribute: A character string that names an attribute of object.
#       value: The value that will be assigned to the attribute slot of 
#              object.
# Returns:
#   Nothing.  Rather, the attribute slot of object is modified in place.
#' @export
`%@%<-` <- function(object, attribute, value) `attr<-`(object, attribute, value)









#' Make a function to filter out trials based on attribute values
#' 
#' This is a curried function.
#' 
#' @param attr_name the name of attribute to filter by
#' @param trials a list of Trial objects
#' @param attr_value a vector of values to filter out
#' @return a function `f(trials, attr_values)` that filters out trials with the
#'   specified `attr_values` for the given `attr_name`.
MakeAttributeFilter <- function(attr_name) {
  function(trials, attr_value) {
    classes <- class(trials)
    trials <- trials[!is.element(trials %@% attr_name, attr_value)] 
    class(trials) <- classes
    trials
  }
}




#' Curry the attribute fetcher function for specific attribute
#' 
#' Originally, one had to write the several functions to retrieve some attribute
#' of a trial or set of trials. E.g., getting the `WordGroup` attribute
#' required:
#' 
#' ```
#' WordGroup <- function(x, ...) UseMethod('WordGroup') 
#' WordGroup.Trial <- function(trial) {attr(trial, 'WordGroup')} 
#' WordGroup.list <- function(list) {mapply(WordGroup, list)}
#' ``` 
#' 
#' The `%@@%` methods by Pat Reidy obviated this problem by providing a general 
#' function for a getting/setting attributes from trials or lists of trials. 
#' Named functions like `StimType(...)` or `WordGroup(...)` are still useful and
#' may be lurking in older eye-tracking scripts. The `FetchAttr` is a closure 
#' function that returns a named function for retrieving a specified attribute.
#' 
#' @param attribute a character string naming an attribute
#' @param object in the returned function, the object whose attributes we query.
#' @return a function that returns the value of `attribute` 
FetchAttr <- function(attribute) {
  function(object) object %@% attribute
}

# `FetchAttr` is used below to recreate all of the original named functions. The 
# `trials` argument may be a `list` of trials, a `Trial`, a `Block` or a
# `Session` onject.

# Experiment and stiumli attributes
Attention      <- FetchAttr("Attention")
ImageL         <- FetchAttr("ImageL")
ImageR         <- FetchAttr("ImageR")
StimType       <- FetchAttr("StimType")
# Subject        <- FetchAttr("Subject")
TargetImage    <- FetchAttr("TargetImage")
TargetWord     <- FetchAttr("TargetWord")
TrialNo        <- FetchAttr("TrialNo")
WordGroup      <- FetchAttr("WordGroup")

# Event-timing attributes (in order of presentation during a Trial)
ImageOnset       <- FetchAttr("ImageOnset")
FixationOnset    <- FetchAttr("FixationOnset")      
FixationDur      <- FetchAttr("FixationDur")        
CarrierOnset     <- FetchAttr("CarrierOnset")
CarrierEnd       <- FetchAttr("CarrierEnd")
TargetOnsetDelay <- FetchAttr("TargetOnsetDelay")   
TargetOnset      <- FetchAttr("TargetOnset")
TargetEnd        <- FetchAttr("TargetEnd")
AttentionOnset   <- FetchAttr("AttentionOnset")
AttentionEnd     <- FetchAttr("AttentionEnd")

# These attributes are only used in AOI.log.odds objects
ZeroBins        <- FetchAttr("ZeroBins")
NonZeroBins     <- FetchAttr("NonZeroBins")

# Deprecated?
# AudioStim      <- FetchAttr("AudioStim")
# DistractorAOI  <- FetchAttr("DistractorAOI")
# LeftAOI        <- FetchAttr("LeftAOI")
# RightAOI       <- FetchAttr("RightAOI")
# TargetAOI      <- FetchAttr("TargetAOI")