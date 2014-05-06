

#' Extract columns from a data-frame
#'
#' `.ExtractDataFrameCols` is a utility function for extracting columns from
#' a data-frame. This function is curried so that a character vector of column
#' names can be passed and the resulting function applied to each element of a
#' `list` of `data.frames`.
#'
#' @param columnNames A character vector, whose elements each name a distinct
#'   column in the `dframe` argument.
#' @param copyAttributes: A boolean. If `TRUE`, then the attributes of
#'   `dframe`, other than `$names`, `$class` and `$row.names` are copied over
#'   to the `data.frame` that results from extracting columns from dframe. If
#'   `FALSE`, then none of dframe's attributes are copied over to the
#'   extracted columns.
#' @param dframe A `data.frame` object.
#' @return When passed the `columnNames` argument, a `function` that when
#'   applied to a `data.frame` returns its columns whose names are found in
#'   `columnNames`.
.ExtractDataFrameCols <- function(columnNames, copyAttributes = TRUE) {
  # The function that results from passing a `columnNames` argument.
  .LambdaDataFrame <- function(dframe) {
    # Extract the columns of the `data.frame`.
    extracted.cols <- dframe[, columnNames, drop = FALSE]
    # The attributes of dframe aren't automatically extracted with the
    # columns. If the user chooses to copy these attributes, do so.
    if (copyAttributes) {
      # Copy dframe's attributes.
      new.attrs <- attributes(dframe)
      # The $names, $class, and $row.names attributes need to be changed.
      new.attrs$names <- names(extracted.cols)
      new.attrs$class <- class(extracted.cols)
      new.attrs$row.names <- row.names(extracted.cols)
      # Set the attributes of extractedCols to new.attrs.
      attributes(extracted.cols) <- new.attrs
    }
    # Return the extracted columns.
    return(extracted.cols)
  }
  # Return the .LambdaDataFrame function.
  return(.LambdaDataFrame)
}





.FindLooksToAOICodes <- function(aoiCodesMapToOne) {
  # .FindLooksToAOICodes is a utility function for transforming the values of
  # each element in an R object from the discrete AOI values to either 1 or 0
  # according to whether the value at that frame matches one of a particular
  # set of AOI codes.  This function is curried so that a numeric vector of
  # AOI codes can be passed and the resulting function applied to each element
  # of a list of R objects that each represent discretized AOI data.
  # Arguments:
  #   aoiCodesMapToOne: A vector that specifies which values in aoiData should
  #                     be transformed to 1.  Any value in aoiData that is not
  #                     in aoiCodesMapToOne is mapped to 0.
  #            aoiData: An R object that represents discretized AOI data.
  # Returns:
  #   When passed an aoiCodesMapToOne, a function that is missing an aoiData
  #   argument.  When the aoiData is subsequently passed, an R object that has
  #   the same class as aoiData.  Each value of aoiData is mapped to either 1 or
  #   0 depending on whether that value is an element of aoiCodesMapToOne or not,
  #   respectively.
  
  # The function that results from passing an aoiCodesMapToOne argument.
  .LambdaAOIData <- function(aoiData) {
    # A utility function that transforms each occurrence of one specific value
    # to 1, and all other values to 0.
    .MapValueToOne <- function(value) {
      # Make a copy of aoiData.
      mapped.data <- aoiData[[1]]
      # If value isn't NA:
      if (! is.na(value)) {
        # Map the occurrences of value to 1.
        mapped.data[aoiData == value] <- 1
        # Map all other values to 0.
        mapped.data[aoiData != value | is.na(aoiData)] <- 0
      } else {
        # Map all missing values to 1.
        mapped.data[is.na(aoiData)] <- 1
        # Map all non-missing values to 0.
        mapped.data[! is.na(aoiData)] <- 0
      }
      mapped.data <- as.numeric(mapped.data)
      # Return the mapped data.
      return(mapped.data)
    }
    # Apply .MapValueToOne to each element in aoiCodesMapToOne.
    list.of.mapped.data <- Map(.MapValueToOne, aoiCodesMapToOne)
    # Reduce the list of mapped data to an R object that has the same class as
    # aoiData by summing the elements of list.of.mapped.data.
    mapped.aoi.data <- Reduce(`+`, list.of.mapped.data)
    # Return the mapped AOI data.
    return(mapped.aoi.data)
  }
  
  # Return the .LambdaAOIData function.
  return(.LambdaAOIData)
}





#' Combine Rows in a Data-frame Together into Bins
#'
#' `.BinAndSumValues` is a utility function for repeatedly summing across a
#' certain number of rows in a data-frame, in such a way that each row enters
#' into only one sum.
#'
#' @param dframe A `data.frame` object with `numeric` data..
#' @param bin_width An integer that determines how many rows of the `data.frame`
#'   argument enter into each sum.
#' @return A `data.frame` whose values represent the row sum of each bin.  The
#'   number of rows in the returned `data.frame` is equal to:
#'   `floor(num.rows(dframe) / bin_width)`.
.BinAndSumValues <- function(dframe, bin_width) {
  dframe <- as.data.frame(dframe)
  # Get the number of rows and columns in the data-frame.
  num_rows <- nrow(dframe)
  num_cols <- ncol(dframe)
  
  # Determine the number of bins that can fit in the data-frame.
  num_bins <- floor(num_rows / bin_width)
  leftover <- num_rows %% bin_width
  
  # Drop the remainder rows **from the top of the data-frame**
  if (leftover != 0) {
    dframe <- dframe[-seq_len(leftover), ]
  }
  
  bin_indices <- sort(rep(seq(from = 1, to = num_bins), times = bin_width))
  
  # Sum up the data in each bin
  dframe[, ncol(dframe) + 1] <- bin_indices
  last_col <- names(dframe)[ncol(dframe)]
  binned <- ddply(dframe, last_col, colSums)
  
  binned <- binned[order(binned[last_col]), ]
  binned[last_col] <- NULL
  binned
}





#' Compute the Log-Odds Ratio of Looking to Target versus Non-target Areas of
#' Interest.
#'
#' The value of the log-odds ratio for each time-point is found by:
#' 1. binning the adjacent time-points in each trial in a way such that the bins
#'    do not overlap,
#' 2. summing all the looks to the "target" AOI in each bin across all trials in
#'    `trials`, call this `target_sum`;
#' 3. summing all the looks to a "distractor" AOI in each bin across all trials,
#'    call this `distract_sum`;
#' 4. correcting values of `target_sum` and `distract_sum`;
#' 5. computing `log(target_sum / distract_sum)`.
#'
#' The times at which each log-odds ratio is defined are found by taking the
#' mean time value of each bin. The times and values of the log odds ratio
#' function(s) are collected into a `data.frame`. The first column of the
#' `data.frame` is the times, each other column is a log-odds ratio.
#'
#' @param trials A list of `Trial` objects that have been aligned (via 
#'   `AlignTrials`) and had discretized AOI data added (via `AddAOIData`).
#' @param aoi_column A character vector whose elements each name a column of
#'   discretized AOI data in all the `Trial` objects in `trials`. Default is
#'   `'GazeByImageAOI'`.
#' @param target_aoi A vector that specifies which AOI codes should be counted
#'   as looks to target---i.e., which AOI codes should be summed as the
#'   numerator of the log odds ratio. Default is `"Target"`, which counts only
#'   the looks to the target image in the `GazeByImageAOI` discretization.
#' @param distractor_aoi Vector that specifies which AOIs should be
#'   counted as looks to the "distractor" image, or perhaps more correctly,
#'   looks away from target---i.e., which AOI codes should be summed as the
#'   denominator of the log odds ratio.
#' @param bin_width An integer specifying the number of adjacent frames that 
#'   should be binned and used to compute the log odds ratio of the discretized 
#'   data.
#' @return A 2--column `data.frame` of binned gaze data enumerating (1) the mean
#'   time of each bin and (2) the log-odds ratio of looks to each type of 
#'   discretized AOI data specified by `aoi_column`.
#'   
#' @note Note: For the parameter `distractor_aoi`, it is necessary to pay 
#'   attention to the possible values of the discretized AOI data for which you 
#'   are computing the log-odds ratio.  For example, `XMeanAOI` and `YMeanAOI` 
#'   take values -1, 0, 1, and NA; so, the default value of `distractor_aoi`
#'   would be nonsensical for `XMeanAOI` and `YMeanAOI`. See the documentation 
#'   for `AddAOIData.Trial` for a description of the different AOI data and the 
#'   possible values that each can take.
AOILogOddsRatio <- function(trials, aoi_column = "GazeByImageAOI",
                            target_aoi = c("Target"),
                            distractor_aoi = c("SemanticFoil", "PhonologicalFoil", "Unrelated"), 
                            bin_width = 3) {
  # Extract the columns in `aoi_column` from each Trial in `trials`.
  list_of_aoi_data <- Map(.ExtractDataFrameCols(aoi_column), trials)
  
  # Transform the values of each element in `list_of_aoi_data` from the discrete
  # AOI values to either 1 or 0 according to whether the value at that frame
  # counts as a look to one of the AOI codes in `target_aoi` or not,
  # respectively.
  looks_to_target_by_trial <- Map(.FindLooksToAOICodes(target_aoi), 
                                  list_of_aoi_data)
  # For each timepoint in each trial, looks-to-target are coded as 1 and
  # looks-elsewhere are coded as 0. Now we fold the trials together to get the
  # total number of looks-to-target for each time point, by summing the elements
  # of looks_to_target_by_trial
  total_looks_to_target <- Reduce(`+`, looks_to_target_by_trial)
  
  # Repeat the last two steps, but for the looks to the foils---i.e., find the
  # total looks relative to which the log odds of the total looks to target is
  # computed.
  looks_to_foils_by_trial <- Map(.FindLooksToAOICodes(distractor_aoi),
                                 list_of_aoi_data)
  total_looks_to_foils <- Reduce(`+`, looks_to_foils_by_trial)
  
  # The looks-to-target and looks-to-foils each are partitioned into time bins
  # and the total looks per bin is summed.
  binned_looks_to_target <- .BinAndSumValues(total_looks_to_target, bin_width)
  binned_looks_to_foils <- .BinAndSumValues(total_looks_to_foils, bin_width)
  
  # #### Continuity correction
  
  # Make a map of zeroes (empty bins) in the looks-to-target and looks-to-foils.
  zero_foils <- as.numeric(binned_looks_to_foils == 0)
  zero_target <- as.numeric(binned_looks_to_target == 0)
  
  # Add the maps together. Non-zero values here are correspond to empty bins.
  zeroes <- zero_target + zero_foils
  
  # Store indices of empty bins, so we know which bins had to be corrected.
  ZeroBins <- which(zeroes != 0)
  NonZeroBins <- which(zeroes == 0)
  
  # Add .5 to all values. This is our "continuity correction". It keeps us from
  # dividing by zero when we compute the log-odds ratio.
  binned_looks_to_target <- binned_looks_to_target + .5
  binned_looks_to_foils <- binned_looks_to_foils + .5
  
  # Compute the log odds ratios.
  log_odds_ratios <- log(binned_looks_to_target / binned_looks_to_foils)
  names(log_odds_ratios) <- "LogOdds"
  
  # #### Find the average time value for each point in the log-odds ratio.
  
  # First, extract the 'Time' column from the first `Trial` object in
  # `trials`.  Each element of `trials` should have the same values
  # in their respective 'Time' column, so taking the first element of
  # `trials` is fine.
  time.values <- .ExtractDataFrameCols('Time')(trials[[1]])
  # Second, bin and sum the time.values.
  binned.time.values <- .BinAndSumValues(time.values, bin_width)
  # Lastly, divide each element of binned.time.values by the bin_width to
  # get the average time value for that bin.
  mean.time.values <- binned.time.values / bin_width
  
  # Columm-bind the mean time values and the log-odds ratios into a single
  # data.frame.
  log_odds_ratios <- cbind(mean.time.values, log_odds_ratios)
  
  
  # #### Add sensible attributes to the data-frame of log-odds ratio. These
  # shortcut functions take attributes from `trials` and attach them onto
  # the log-odds data-frame. They are used just to make the code more readable.
  .AddUniqueAttribute <- function(t_attr) {
    log_odds_ratios %@% t_attr <- unique(trials %@% t_attr)
    return(log_odds_ratios)
  }
  .AddMeanAttribute <- function(t_attr) {
    log_odds_ratios %@% t_attr <- mean(trials %@% t_attr)
    return(log_odds_ratios)
  }
  
  log_odds_ratios <- .AddUniqueAttribute("Task")
  log_odds_ratios <- .AddUniqueAttribute("Subject")
  log_odds_ratios <- .AddUniqueAttribute("StimType")
  log_odds_ratios <- .AddUniqueAttribute("TargetWord")
  log_odds_ratios <- .AddUniqueAttribute("AlignedBy")
  
  log_odds_ratios <- .AddMeanAttribute("ImageOnset")
  log_odds_ratios <- .AddMeanAttribute("CarrierOnset")
  log_odds_ratios <- .AddMeanAttribute("CarrierEnd")
  log_odds_ratios <- .AddMeanAttribute("TargetOnset")
  log_odds_ratios <- .AddMeanAttribute("TargetEnd")
  log_odds_ratios <- .AddMeanAttribute("AttentionOnset")
  log_odds_ratios <- .AddMeanAttribute("AttentionEnd")
  log_odds_ratios <- .AddMeanAttribute("FixationOnset")
  
  # Also attach which bins were empty and not empty.
  log_odds_ratios %@% 'ZeroBins' <- ZeroBins
  log_odds_ratios %@% 'NonZeroBins' <- NonZeroBins
  
  # Update the class of the data-frame and return.
  class(log_odds_ratios) <- c('AOI.LogOdds', 'data.frame')
  return(log_odds_ratios)
}




