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
        expect_true(all(TargetOnset < AttentionOnset))
      })
    })

    test_that("Events add up", {
      with(stimdata, {
        expect_equivalent(AttentionEnd, AttentionOnset + AttentionDur)
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




























#
#
#
#
#
#
#
#
# # Code I've used for testing the stimdata function.
# stimfiles <- dir("inst/tests/data", full.names = TRUE, pattern = "*.txt",
#                  recursive = TRUE)
# stim <- lapply(stimfiles, LoadStimdataFile)
# stim_config <- lapply(stim, DetermineStim)
# Stimdata(stimfiles[[1]])
# lapply(stimfiles, Stimdata)
#
# DetermineStim(stim[[1]])
# ParseFilename(stimfiles[[1]])
# basename(stimfiles[[1]])
# ParseFilename

# # Test values
# stimdata_path <- "inst/tests/data/RWL_NoFixations_CS1/001C48FS1/RWL_Block1_001C48FS1.txt"
# stimdata_path <- "inst/tests/data/Coartic_WFFArea_2a/326P21MS1/Coartic_Block1_326P21MS1.txt"
# stimdata_path <- "inst/tests/data/MP_NoFixations_CS1/001C45FS1/MP_Block1_001C45FS1.txt"
# stimdata_path <- "inst/tests/data/MP_NoFixations_CS1/086C58FA1/MP_Block1_086C48FA1.txt"
# stimlog <- LoadStimdataFile(stimdata_path)
# stim_config <- MakeStimdataConfig(named_stim)
# stim_config <- DetermineStim.Coartic(stimlog)


