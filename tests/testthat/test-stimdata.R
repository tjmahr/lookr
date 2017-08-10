
context("Fetching values from Eprime files")

stimdata_path <- test_path("data/MP_NoFixations_CS1/001P00XA1/MP_Block2_001P00XA1.txt")
stimlog <- suppressMessages(LoadStimdataFile(stimdata_path))

test_that("Handling dates from Eprime files", {
  stim <- Stimdata(stimdata_path)
  expect_true(stim$DateTime[1] == "2013-01-11 13:08:54")
})


test_that("Get Values", {
  # Works as expected. Test on experimental conditions.
  stim_types <- .GetValuesOfStimdataType(stimlog)("StimType")
  expect_equivalent(stim_types[1:3], c("FAM", "FAM", "real"))

  # Test on trial numbers
  trials <- as.numeric(.GetValuesOfStimdataType(stimlog)("TrialList"))
  # Trials from 1 to the number of the last trial, so the extract trial numbers
  # be the same as their position in the vector.
  expect_equivalent(trials, seq_along(trials))

  # Input validation. Only one element should be extracted.
  expect_error(.GetValuesOfStimdataType(stimlog)(c("TrialList", "StimType")))

  # Missing fields return empty vectors
  fake_field <- .GetValuesOfStimdataType(stimlog)("FakeField")
  expect_equal(length(fake_field), 0)

  # Multiple elements are extracted with Map.
  multiple <- c("TrialList", "StimType")
  values <- Map(.GetValuesOfStimdataType(stimlog), multiple)
  expect_equal(values[["TrialList"]][1], "1")
  expect_equal(values[["StimType"]][1], "FAM")
})

test_that("Check Values", {
  expect_error(.CheckForStimdataType(stimlog, c("TrialList", "StimType")))
  expect_true(.CheckForStimdataType(stimlog, "TrialList"))
  expect_false(.CheckForStimdataType(stimlog, "FakeField"))
})


test_that("Writing stimdata csvs", {
  # No csv written by default
  expected_csv <- paste0(tools::file_path_sans_ext(stimdata_path), "_stim.csv")
  stim <- Stimdata(stimdata_path)
  expect_false(file.exists(expected_csv))

  # csv written
  stim <- Stimdata(stimdata_path, output_file = TRUE)
  expect_true(file.exists(expected_csv))

  # loaded csv is the same the original dataframe
  stim_from_csv <- read.csv(expected_csv, stringsAsFactors = FALSE)
  for (col_name in colnames(stim)) {
    expect_equal(stim[[col_name]], stim_from_csv[[col_name]])
  }

  file.remove(expected_csv)
  expect_false(file.exists(expected_csv))

  # csv written lookr opt is set
  lwl_opts$set(write_stimdata = TRUE)
  stim <- Stimdata(stimdata_path)
  expect_true(file.exists(expected_csv))

  file.remove(expected_csv)
  expect_false(file.exists(expected_csv))

  # revert option. no file written
  lwl_opts$restore()
  stim <- Stimdata(stimdata_path)
  expect_false(file.exists(expected_csv))
})



# This scripts performs a number of tests against the `Stimdata` function, which
# loads and extracts relevant information from an Eprime-outputted text file.
#
# These tests are important because the `Stimdata` function provides trial-level
# information about the stimuli presented during an eye-tracking experiment and
# calculates information about the timing of experimental evens from the
# extracted information.
#
# In order to pass these tests, the function must load and check certain
# conditions on some sample stimdata from _every_ Looking While Listening
# experiment we've developed in the lab.

library("reshape2")

# Get all the subject-level directories for each task.
experiments <- dir("data", full.names = TRUE)
test_subjects <- lapply(experiments, dir, pattern = "[0-9]{3}[C|L|P]")

# Exclude elements with no subfolders
names(test_subjects) <- dir("data")
test_subjects <- test_subjects[sapply(test_subjects, length) > 0]


# Reduce this list of subjects into a dataframe with subject, task, protocol,
# and experiment information.
tests <- melt(test_subjects, value.name = "Subject")
names(tests)[2] <- "Experiment"
tests <- tests[, 2:1]

TaskTest <- function(experiment, subject, ...) {
  path <- file.path("data", experiment, subject)
  subject <- as.character(subject)
  parsed_experiment <- ldply(str_split(experiment, "_"))
  task <- parsed_experiment[, 1]
  protocol <- parsed_experiment[, 2]

  structure(list(
    Experiment = experiment,
    Path = path,
    ExpSubj = sprintf("%s (%s)", experiment, subject),
    Subject = subject,
    Task = task,
    Protocol = protocol,
    StimFiles = dir(path, pattern = "*.txt", full.names = TRUE)),
    class = c("TaskTest", "list"))
}


task_tests <- Map(TaskTest, tests$Experiment, subject = tests$Subject)



# Test each subject in each experiment
for (task_test in task_tests) {
  # Run tests
  context(task_test$ExpSubj)

  # Test each stim file for the subject
  for (stim in task_test$StimFiles) {
    # Compute the stimdata, suppressing messages.
    stimdata <- suppressMessages(Stimdata(stim))

    test_that("Stimdata class includes 'Stimdata' and the task name", {
      expect_is(stimdata, task_test$Task)
      expect_is(stimdata, "Stimdata")
    })

    test_that("Events are orderly", {
      with(stimdata, {
        expect_true(all(ImageOnset < CarrierOnset))
        expect_true(all(CarrierOnset < TargetOnset))
      })
    })

    test_that("Events add up", {
      with(stimdata, {
        expect_equivalent(CarrierEnd, CarrierOnset + CarrierDur)
        expect_equivalent(TargetEnd, TargetOnset + TargetDur)
        expect_equivalent(TargetOnset, CarrierOnset + CarrierDur +
                            DelayTargetOnset)
      })
    })

    test_that("Fixation is present/absent", {
      if (task_test$Protocol == "NoFixations") {
        expect_false("FixationOnset" %in% names(stimdata))
      } else {
        expect_true("FixationOnset" %in% names(stimdata))
      }
    })
  }
}
