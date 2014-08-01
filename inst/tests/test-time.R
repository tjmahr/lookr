context("Time related functions")
trials <- suppressMessages(Session("data/RWL_WFFArea_Long/001P00XS1/"))

test_that("TimeSlice works on correctly for different input types", {
  trial <- trials[[1]]
  trial <- AdjustTimes(trial, "TargetOnset")

  # Handle weird times by using min and max
  weird_times <- TimeSlice(trial, NA, NULL)
  expect_equivalent(range(trial$Time), range(weird_times$Time))

  # When slicing by events, time values are within one frame of the event times
  sample_rate <- mean(diff(trial$Time))
  event_times <- TimeSlice(trial, "CarrierOnset", "AttentionOnset")
  onsets <- c(trial %@% "CarrierOnset", trial %@% "AttentionOnset")
  expect_equal(onsets, range(event_times$Time), tol = sample_rate)

  # Same thing for numeric arguments
  num_times <- TimeSlice(trial, -200, 2000)
  expect_equal(c(-200, 2000), range(num_times$Time), tol = sample_rate)
})


test_that("Vectorized TimeSlice", {
  same_number_of_frames <- function(xs) {
    frames <- xs %@% "NumberOfFrames"
    all(frames[1] == frames)
  }
  trials <- AdjustTimes(trials, "TargetOnset")

  # Trials are same duration when slicing using events with fixed duration
  smooth1 <- TimeSlice(trials, -500, 2000)
  smooth2 <- TimeSlice(trials, "TargetOnset", "TargetEnd")
  expect_true(same_number_of_frames(smooth1))
  expect_true(same_number_of_frames(smooth2))

  # Trials are not same duration when slicing events with variable durations
  ragged <- TimeSlice(trials, "ImageOnset", "TargetOnset")
  expect_false(same_number_of_frames(ragged))
})


test_that("AdjustTimes updates times and event attributes", {
  trial <- trials[[1]]
  onsets <- c("Image", "Fixation", "Carrier", "Target", "Attention")
  onsets <- paste0(onsets, "Onset")
  get_onsets <- function(x) mapply(function(y) x %@% y, onsets)

  # Raw event times = adjusted times + adjustment factor
  raw_onsets <- get_onsets(trial)
  adj_onsets <- get_onsets(AdjustTimes(trial, "TargetOnset"))
  expect_true(all(raw_onsets == (adj_onsets + raw_onsets["TargetOnset"])))

  # Adjustment anchors are time 0
  adjustment_anchors <- AdjustTimes(trials, "TargetOnset") %@% "TargetOnset"
  expect_true(all(adjustment_anchors == 0))

  # Make sure updated times have nearly same sampling rate as unadjusted times
  raw_sample_lags <- diff(trial$Time)
  adj_sample_lags <- diff(AdjustTimes(trial, "TargetOnset")[["Time"]])
  raw_rate <- mean(raw_sample_lags)
  adj_rate <- mean(adj_sample_lags)
  # Within .01 units
  expect_less_than(abs(raw_rate - adj_rate), .01)
})


test_that("FindClosestFrame selects earliest and nearest row to a value", {
  FindClosestFrame <- lookr:::FindClosestFrame
  # Times should be unique and in increasing order
  normal_time <- data.frame(Time = c(-3, -1, 1, 3))
  expect_equal(FindClosestFrame(normal_time, -1), 2)
  expect_equal(FindClosestFrame(normal_time, 0), 2)

  # Weird case where time is unordered and values aren't unique
  weird_time <- data.frame(Time = c(1, 2, 3, 2, -1, -3))
  expect_equal(FindClosestFrame(weird_time, -1), 5)
  expect_equal(FindClosestFrame(weird_time, 0), 5)
  expect_equal(FindClosestFrame(weird_time, 2), 2)
})
