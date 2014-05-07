

PrimeData <- function(stimdata_path, selects = NULL, drops = NULL) {
  require(stringr)
  # We need to know which blocks of text to extract.
  if(is.null(selects) & is.null(drops)) {
    stop("No value for \"Running: \" specified. ")
  }
  
  # Load the file
  stimlog <- LoadStimdataFile(stimdata_path)
  
  # Find all the texts between LogFrame boundaries
  frame_starts <- "\\*\\*\\* LogFrame Start \\*\\*\\*"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
  frame_ends <- "\\*\\*\\* LogFrame End \\*\\*\\*"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
  starts <- which(str_detect(stimlog, frame_starts))
  ends <- which(str_detect(stimlog, frame_ends))
  ranges <- mapply(":", starts + 1, ends - 1)
  
  # Extract those texts
  ExtractStim <- function(lines) stimlog[lines]
  stim_sets <- lapply(ranges, ExtractStim)
  stim_sets <- lapply(stim_sets, str_trim)
  
  # Extract only sets with a "Running: " unit
  DetectRunning <- function(set) TRUE %in% str_detect(set, "Running")
  runnings <- sapply(stim_sets, DetectRunning)  
  stim_sets <- stim_sets[runnings]  
  
  # Convert a colon-space delimited set into a list of named strings
  Listify <- function(set) {
    splits <- str_split_fixed(set, pattern=": ", 2)
    set <- splits[, 2]
    names(set) <- splits[, 1]
    as.list(set)
  }
  stim_lists <- lapply(stim_sets, Listify)
  
  # Look at the "Running: ". Select only those in `selects` or those not in `drops`.
  ExtractRunning <- function(stim_list) stim_list$Running
  chunks <- sapply(stim_lists, ExtractRunning)
  chunk_names <- unique(chunks)
  
  if(!is.null(drops)) chunk_names <- setdiff(chunk_names, drops)
  if(!is.null(selects)) chunk_names <- chunk_names[chunk_names %in% selects]
  
  # Turn those text-lists into dataframe rows
  keepers <- which(chunks %in% chunk_names)
  MakeRow <- function(i) data.frame(i)
  stim_lists <- stim_lists[keepers]
  rows <- lapply(stim_lists, MakeRow)
  
  # Bind the rows together
  stim_frame <- Reduce(rbind, rows)
  
  return(stim_frame)
}
