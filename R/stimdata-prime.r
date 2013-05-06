file <- "inst/tests/data/Coartic_WFFArea_1a/302P22MS1/Coartic_Block1_302P22MS1.txt"
gaze_file <- "inst/tests/data/Coartic_WFFArea_1a/302P22MS1/Coartic_Block1_302P22MS1.gazedata"


stim <- PrimeData(file, "TrialList")
 









DropColumns <- function(dataframe) {
  
  CheckThenDrop <- function(dataframe, columns) {
    if(length(columns) == 0) {
      dataframe
    } else {
      subset(dataframe, select = -columns)  
    }
  }
  
  # Drop ".StartTime"
  start_times <- which(unlist(str_detect(names(stim), ".StartTime")) == TRUE)
  stim <- CheckThenDrop(stim, columns = c())  
  
  # Drop specific columns
  ignore <- c("Procedure", "Running", "TrialList.Cycle", "TrialList.Sample")
  ignore <- ignore[ignore %in% names(stim)]
  ignore_cols <- which(names(stim) %in% ignore)
  stim <- CheckThenDrop(stim, ignore_cols)
  stim
}



# Rename columns
RenameColumns <- function(dataframe) {
  
}

ConfigureTask <- function(...) UseMethod("ConfigureTask")
ConfigureTask.default <- function(dataframe) dataframe


ConfigureTask.MP <- function(dataframe) {
  
}






# Derive values