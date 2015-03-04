# Functions to support the creation of stimdata files from eprime text files

#' Use the values in one column to extract values from other columns.
#'
#' @param df a data-frame
#' @param col_name a string naming a column whose values are the names of the
#'   other columns in the data-frame.
#' @return a vector of values from the columns named in df$col_name
#' @examples
#' # The Location column names other columns
#' df <- data.frame(stringsAsFactors = FALSE,
#'   Location = c("Left", "Right", "Right", "Left"),
#'   Left = c("l1", "l2", "l3", "l4"),
#'   Right = c("r1", "r2", "r3", "r4"))
#' teleport_by_column(df, "Location")
#' # [1] "l1" "r2" "r3" "l4"
#' # We use the values in "Location" to teleport into those other columns
teleport_by_column <- function(df, col_name) {
  rows <- seq_len(nrow(df))
  # Are the values in the pathway-column also column names?
  values <- df[[col_name]]
  stopifnot(all(values %in% colnames(df)), length(col_name) == 1)
  # Curry the `x[i,j]` function so we can fix the x parameter and map over the
  # is, js
  extract_ij <- function(x) function(i, j) x[i, j]
  hits <- Map(extract_ij(df), i = rows, j = values) %>% unlist
  hits
}

flip_image <- function(xs) ifelse(xs == "ImageL", "ImageR", "ImageL")

# Extract the date and time lines from the header
get_eprime_datetime <- function(framelist) {
  visit <- framelist %>%
    filter_in("Running", "Header") %>%
    to_data_frame %>%
    extract(c("SessionDate", "SessionTime")) %>%
    as.list

  # Parse the date/time, and store as string
  visit$Date <- mdy(visit$SessionDate, quiet = TRUE, tz = "America/Chicago")
  visit$Time <- hms(visit$SessionTime)
  datetime <- as.character(visit$Date + visit$Time)
  datetime
}

# Convert a vector to numeric, unless the conversion would raise a warning
try_numeric <- function(xs) {
  # Convert to character first in case it's a factor
  ys <- tryCatch(as.numeric(as.character(xs)), warning = function(w) w)
  if (inherits(ys, "warning")) xs else ys
}

try_all_numerics <- . %>% lapply(., try_numeric) %>% as_data_frame








## Old helper functions (from the original stimdata*.R files)

# Break path/to/TASK_BlockNUM_SUBJECT.ext into TASK, NUM, SUBJECT.
parse_l2t_filename <- function(filename) {
  file_basename <- file_path_sans_ext(basename(filename))

  # Extract the fields from the basename.
  file_info <- unlist(str_split(file_basename, pattern = "_"))
  task <- file_info[1]
  block_name <- str_extract(file_basename, "Block[0-9]{1}")

  # `block_name` is "Block1" or "Block2" right now. We just want the number.
  block <- as.integer(str_extract(block_name, pattern = "[1-9]"))

  # The [MFX] field includes X to match the files in the dummy/test data
  subject <- str_extract(file_basename, "[0-9]{3}[A-Z][0-9]{2}[MFX][AS][1-9]{1}")

  # Bundle these four data together
  list(Task = task, Block = block, Subject = subject, Basename = file_basename)
}

# Determine the protocol described in the raw stimdata file.
#
# As of Oct 2012, we have 3 different protocols/versions each of the RWL and
# MP tasks, each with different stimdata files that need to be handled in
# slightly different ways. By checking if the stimdata contains a Fixation
# Onset field and/or a Fixation Movie field, we can figure out which specific
# version of the RWL or MP protocol is represented in the stimdata file. Here
# is the cheatsheet:
# ```
#   PROTOCOL      DATE        Fixations?    Fixation Movies?
#   NoFixations   Mar 2012    No            No
#   WFF_Movie     Jul 2012    Yes           Yes
#   WFF_Area      Oct 2012    Yes           No
# ```
determine_protocol <- function(stimdata_df) {
  movs <- any(str_detect("FixationMovie", names(stimdata_df)))
  time <- any(str_detect("FixationOnset", names(stimdata_df)))
  if (movs) "WFF_Movie" else if (time) "WFF_Area" else "NoFixations"
}
