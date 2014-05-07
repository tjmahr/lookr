


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
#' image (\code{Target}), looks to distractor image(s) (\code{Others}), number
#' of missing looks (\code{NAs}), number of tracked looks that don't fall in an
#' AOI (\code{Elsewhere}), and the proportion of looks to target versus
#' competing AOIs (\code{Proportion}).
#' 
#' @param frame a dataframe of melted looking data, containing a 
#'   \code{GazeByImageAOI} column.
#' @param formula a \code{dcast} formula for aggregating the looking data. The 
#'   default is \code{Subj + Condition + Time ~ GazeByImageAOI}
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


#' Assign bin numbers to a vector
#' 
#' The first step in binning/down-sampling some data is assigning items to bins.
#' This function takes a vector and a bin size and returns the bin assignments.
#' 
#' @param xs a vector
#' @param bin_width the number of items to put in each bin. Default is 3.
#' @param na_location Where to assign \code{NA} bin numbers. \code{"head"} and
#'   \code{"tail"} respectively put the NA elements at the head and tail of the
#'   vector; \code{"split"} alternates between \code{"tail"} and \code{"head"}.
#' @return a vector of bin-numbers. If \code{bin_width} does not evenly divide
#'   \code{xs}, the remainder elements are given a bin number of \code{NA}.
#' @export
#' @examples
#' AssignBins(1:14, bin_width = 3, "head")
#' # [1] NA NA  1  1  1  2  2  2  3  3  3  4  4  4
#' AssignBins(1:14, bin_width = 3, "tail")
#' # [1]  1  1  1  2  2  2  3  3  3  4  4  4 NA NA
#' AssignBins(1:7, bin_width = 5, "split")
#' # [1] NA  1  1  1  1  1 NA
#' AssignBins(1:8, bin_width = 5, "split")
#' # [1] NA  1  1  1  1  1 NA NA
AssignBins <- function(xs, bin_width = 3, na_location = "head") {
  num_bins <- floor(length(xs) / bin_width)
  leftover <- length(xs) %% bin_width
  bin_indices <- sort(rep(seq_len(num_bins), times = bin_width))
  if (na_location == "head") {
    bin_indices <- c(rep(NA, leftover), bin_indices)
  } else if (na_location == "tail") {
    bin_indices <- c(bin_indices, rep(NA, leftover))
  } else if (na_location == "split") {
    first <- floor(leftover / 2)
    last <- ceiling(leftover / 2)
    bin_indices <-  c(rep(NA, first), bin_indices, rep(NA, last))
  }
  bin_indices
}
