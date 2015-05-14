


#' Convert Trial and list of Trials into a long data-frame
#'
#' @param trial a Trial object with AOI data added
#' @param trials a list of Trial objects
#' @param other_cols additional data-frame columns to extract from each Trial
#' @param other_attrs additional attributes to pull from each Trial
#' @return a single data-frame containing the Time and GazeByImageAOI data from
#'   the Trial(s) with columns for other attribute values of the Trial(s)
#' @export
MeltLooks <- function(...) UseMethod("MeltLooks")

#' @export
MeltLooks.list <- function(trials, ...) ldply(trials, MeltLooks, ...)

#' @export
MeltLooks.Trial <- function(trial, other_cols = NULL, other_attrs = NULL) {
  # Use the TrialNo attribute rather than the column
  trial$Subj <- substr(trial %@% "Subject", 1, 4)
  trial$TrialNo <- NULL
  expected_cols <- c("Task", "Subject", "Subj", "BlockNo", "Basename", "Time",
                     "GazeByAOI", "GazeByImageAOI", other_cols)
  cols <- intersect(expected_cols, names(trial))

  # Column check
  missing_cols <- setdiff(expected_cols, names(trial))
  if (length(missing_cols) > 0) {
    flat_list <- paste0(missing_cols, collapse = ", ")
    warning("Following columns were expected but not found: ", flat_list)
  }

  # Attribute check
  missing_attrs <- setdiff(other_attrs, names(attributes(trial)))
  if (length(missing_attrs) > 0) {
    flat_list <- paste0(missing_attrs, collapse = ", ")
    warning("Following attributes were expected but not found: ", flat_list)
  }

  # `aliased` is used when we want the column name to differ from the attribute
  # name, using the mapping `list(ColumnName = "AttributeName")`
  aliased <- list(
    Condition = "StimType",
    TargetLocation = "TargetImage",
    DistractorLocation = "DistractorImage",
    PhonologicalLocation = "PhonologicalFoilImage",
    SemanticLocation = "SemanticFoilImage",
    UnrelatedLocation = "UnrelatedImage")

  unaliased <- c(
    "WordGroup", "TargetWord", "ImageL", "ImageR", "Pitch", "EmptyTarget",
    "Carrier", "Audio", "CarrierOnset", "TargetEnd", "TrialNo", "DateTime",
    "UpperLeftImage", "UpperRightImage", "LowerRightImage", "LowerLeftImage",
    "Target", "SemanticFoil", "PhonologicalFoil", "Unrelated",
    "UpperLeftImageStimulus", "UpperRightImageStimulus",
    "LowerRightImageStimulus", "LowerLeftImageStimulus", other_attrs)

  # Combine available attributes into a list of named attributes
  aliased <- aliased[is.element(aliased, names(attributes(trial)))]
  unaliased <- unaliased[is.element(unaliased, names(attributes(trial)))]
  names(unaliased) <- unaliased
  attrs <- c(aliased, as.list(unaliased))

  # Bind attributes as columns
  for (x in names(attrs)) trial[[x]] <- trial %@% attrs[[x]]

  # Drop other columns
  id_vars <- unique(c(cols, names(attrs)))
  trial[id_vars]
}


#' Aggregate looks to target image
#'
#' The returned dataframe has columns for the number of looks to the target
#' image (\code{Target}), looks to distractor image(s) (\code{Others}), number
#' of missing looks (\code{NAs}), number of tracked looks that don't fall in an
#' AOI (\code{Elsewhere}), number of looks total (\code{Looks}), proportion of
#' looks to target versus competing AOIs (\code{Proportion}), and proportion of
#' looks that are NA (\code{PropNA})
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

  mutate(looks,
         Looks = Others + Target + NAs + Elsewhere,
         Proportion = Target / (Others + Target),
         PropNA = NAs / Looks)
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
#' @param partial whether to exclude values that don't fit evenly into bins.
#'   Defaults to FALSE, so that the user is warned if a bin is incomplete.
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
AssignBins <- function(xs, bin_width = 3, na_location = "tail", partial = FALSE) {
  if (is.unsorted(xs)) {
    warning("Elements to be binned are not sorted")
  }

  if (length(xs) != length(unique(xs))) {
    warning("Elements to be binned are not unique")
  }

  num_bins <- floor(length(xs) / bin_width)
  leftover <- length(xs) %% bin_width
  bin_indices <- sort(rep(seq_len(num_bins), times = bin_width))

  if (partial) {
    partial_bin <- rep(max(bin_indices) + 1, leftover)
    bin_indices <- c(bin_indices, partial_bin)
    leftover <- 0
  }

  if (na_location == "head") {
    bin_indices <- c(rep(NA, leftover), bin_indices)
  } else if (na_location == "tail") {
    bin_indices <- c(bin_indices, rep(NA, leftover))
  } else if (na_location == "split") {
    first <- floor(leftover / 2)
    last <- ceiling(leftover / 2)
    bin_indices <-  c(rep(NA, first), bin_indices, rep(NA, last))
  }

  lost_values <- xs[which(is.na(bin_indices))]

  if (length(lost_values) > 0) {
    listed_values <- paste0(lost_values, collapse = (", "))
    warning("Some values were not assigned to a bin: ", listed_values)
  }

  bin_indices
}
