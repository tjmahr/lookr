# install_github("lookr", username="tjmahr")
# library(lookr)

# Load all the R files
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

sourceDir("R/")



library(testthat)


# test_package("lookr")
test_dir("./inst/tests/")

