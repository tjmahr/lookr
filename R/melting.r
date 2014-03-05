


#' Convert Trial and list of Trials into a long data-frame
#' 
#' @param trial a Trial object with AOI data added
#' @param trials a list of Trial objects
#' @return a single data-frame containing the Time and GazeByImageAOi data from
#'   the Trial(s) with columns for the attribute values of the Trial(s)
#' @export
MeltLooks <- function(...) UseMethod("MeltLooks")

#' @export
#' @importFrom plyr ldply
MeltLooks.list <- function(trials) ldply(trials, MeltLooks)

#' @export
MeltLooks.Trial <- function(trial) {
  # Use the TrialNo attribute rather than the column
  trial$Subj <- substr(trial %@% "Subject", 1, 4)
  trial$TrialNo <- NULL
  columns_to_keep <- c("Task", "Subject", "Subj", "BlockNo", "Basename", "Time",  
                       "GazeByAOI", "GazeByImageAOI") 
  # `aliased` is used when we want the column name to differ from the attribute
  # name, using the mapping `list(ColumnName = "AttributeName")`
  aliased <- list(Condition = "StimType", TargetLocation = "TargetImage", 
                  DistractorLocation = "DistractorImage")
  unaliased <- c("WordGroup", "TargetWord", "ImageL", "ImageR", "Pitch", "EmptyTarget",
                 "Carrier", "Audio", "CarrierOnset", "TargetEnd", "TrialNo")
  
  aliased <- aliased[is.element(aliased, names(attributes(trial)))]
  unaliased <- unaliased[is.element(unaliased, names(attributes(trial)))]  
  
  # Bind attributes as columns
  for (x in names(aliased)) trial[x] <- trial %@% unlist(aliased[x])
  for (x in unaliased) trial[x] <- trial %@% x
  # Drop other columns
  id_vars <- c(columns_to_keep, names(aliased), unaliased)
  trial[id_vars]
}


#' Aggregate looks to target image
#' 
#' The returned dataframe has columns for the number of looks to the target 
#' image (`Target`), looks to distractor image(s) (`Others`), number of missing 
#' looks (`NAs`), number of tracked looks that don't fall in an AOI
#' (`Elsewhere`), and the proportion of looks to target versus competing AOIs
#' (`Proportion`).
#' 
#' @param frame a dataframe of melted looking data, containing a 
#'   `GazeByImageAOI` column.
#' @param formula a `dcast` formula for aggregating the looking data. The 
#'   default is `Subj + Condition + Time ~ GazeByImageAOI`
#' @return a dataframe with columns of aggregated looks.
#' @importFrom reshape2 dcast
#' @export
AggregateLooks <- function(frame, formula = Subj + Condition + Time ~ GazeByImageAOI) {
  looks <- dcast(frame, formula = formula, fun.aggregate = length, 
                 value.var = "GazeByImageAOI")
  other_AOIs <- setdiff(frame$GazeByImageAOI, c("Target", "tracked", NA))
  looks$Others <- rowSums(looks[other_AOIs])
  names(looks)[which(names(looks) == "NA")] <- "NAs"
  names(looks)[which(names(looks) == "tracked")] <- "Elsewhere"
  transform(looks, Proportion = Target / (Others + Target))  
}
