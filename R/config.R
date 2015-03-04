



#' Load a config file
#'
#' @import yaml
load_yaml <- function(file_path) {
  require("yaml")
  yaml.load_file(file_path)
}


#' Find YAML config files
#'
#' @param dir_path
#' @return
find_yaml <- function(dir_path = ".") {
  # Look for yaml files.
  yamls <- list.files(dir_path, pattern = ".yaml", full.names = TRUE)
  none_found <- length(yamls) == 0
  # Does this location have an accessible parent?
  searchable <- str_detect(dir_path, "/")
  # Is this a data folder? Are there gazedata files in this folder?
  data_folder <- 0 < length(list.files(dir_path, pattern = ".gazedata"))

  # Try not to be nosy. Search the parent directory only if there is no YAML
  # file in the folder and this is a legitimate data folder.
  if (none_found & searchable & data_folder) {
    parent <- dirname(dir_path)
    yamls <- list.files(parent, pattern = ".yaml", full.names = TRUE)
  }

  if (length(yamls) == 0) {
    stop("Could not find a .yaml config file in ", dir_path)
  }

  yamls
}

# file_path <- stimpath

# config <- load_yaml(file_path = "tests/testthat/data/RWL_WFFArea_Long/RWL.yaml")
unpack_yaml <- function(config) {
  wide <- config$Display$width_pix
  high <- config$Display$height_pix
  AOIs <- lapply(config$AOIs, unpack_AOI, wide, high)
  AOIs
}

unpack_AOI <- function(AOI_list, wide, high) {
  AOI(AOI_list$x_pix, AOI_list$y_pix, wide, high)
}
