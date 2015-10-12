context("Melting trials")

test_that("Single Trial", {
  trials <- Block("data/Coartic_WFFArea_2a/001P00XS1/Coartic_Block1_001P00XS1")
  trial <- trials[[1]]
  trial <- AdjustTimes(trial, "TargetOnset")
  trial <- TimeSlice(trial, 0, 500)
  trial <- AddAOIData(trial)
  trial %@% "Testing" <- TRUE
  trial$TestTrial <- 1

  n_frames <- length(trial$Time)
  default_melt <- MeltLooks(trial)

  # The looking data is augmented with trial attributes. The attribute value is
  # repeated for each frame of eyetracking.
  expect_equal(
    default_melt[["Target"]],
    rep(trial %@% "Target", n_frames))

  # Attributes and columns not hard-coded into function are ignored
  expect_null(default_melt[["TestTrial"]])
  expect_null(default_melt[["Testing"]])

  # But these can be specified manually
  expect_equal(
    MeltLooks(trial, other_attrs = "Testing")[["Testing"]],
    rep(trial %@% "Testing", n_frames))

  expect_equal(
    MeltLooks(trial, other_cols = "TestTrial")[["TestTrial"]],
    trial$TestTrial)

  # At the same time
  both_spec <- MeltLooks(trial, "TestTrial", "Testing")
  expect_equal(both_spec[["TestTrial"]], trial$TestTrial)
  expect_equal(both_spec[["Testing"]], rep(trial %@% "Testing", n_frames))
})

test_that("Multiple Trials", {
  trials <- Block("data/Coartic_WFFArea_2a/001P00XS1/Coartic_Block1_001P00XS1")
  trials <- as.TrialList(trials[1:4])
  trials <- AdjustTimes(trials, "TargetOnset")
  trials <- TimeSlice(trials, 0, 500)
  trials <- AddAOIData(trials)
  trials %@% "Testing" <- TRUE

  set_test_trial <- function(x) {
    x[["TestTrial"]] <- x[["TrialNo"]]
    x
  }
  trials <- trial_lapply(trials, set_test_trial)

  n_frames <- length(trials[[1]]$Time)
  default_melt <- MeltLooks(trials)

  # Looking data is augmented with trial attributes. The attribute value is
  # repeated for each frame of eyetracking.
  expect_equal(
    default_melt[["Target"]],
    rep(trials %@% "Target", each = n_frames))

  # Attributes and columns not hard-coded into function are ignored
  expect_null(default_melt[["TestTrial"]])
  expect_null(default_melt[["Testing"]])

  # But these can be specified manually
  expect_equal(
    MeltLooks(trials, other_attrs = "Testing")[["Testing"]],
    rep(trials %@% "Testing", each = n_frames))

  expect_equal(
    MeltLooks(trials, other_cols = "TestTrial")[["TestTrial"]],
    rep(trials %@% "TrialNo", each = n_frames))

  # At the same time
  both_spec <- MeltLooks(trials, "TestTrial", "Testing")
  expect_equal(
    both_spec[["TestTrial"]],
    rep(trials %@% "TrialNo", each = n_frames))

  expect_equal(
    both_spec[["Testing"]],
    rep(trials %@% "Testing", each = n_frames))
})
