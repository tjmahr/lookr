library("dplyr", warn.conflicts = FALSE)
context("Interpolating missing frames")

# To test interpolation, we create mock trials by recombining rows of looking
# data. First, we need to create get a row for each AOI.
trials <- suppressMessages(Session("data/MP_WFFArea_Long/001P00XS1/"))
trials <- AddAOIData(TimeSlice(AdjustTimes(trials)))
src_trial <- trials[[2]]

# Get the first row with a look each non-NA AOI
rows <- list()
is_gaze <- function(aoi) function(xs) identical(xs, aoi)
rows$Target <- src_trial[Position(is_gaze("Target"), src_trial$GazeByImageAOI), ]
rows$tracked <- src_trial[Position(is_gaze("tracked"), src_trial$GazeByImageAOI), ]
rows$Distractor <- src_trial[Position(is_gaze("Distractor"), src_trial$GazeByImageAOI), ]
# Manually select the NA row
rows$`NA` <- src_trial[80, ]

# Create a trial from a sequence of AOI names
make_trial_from_rows <- function(aoi_set, trial_no = 1) {
  # Replicate the attributes from the original src_trial
  trial_attrs <- attributes(as.list(src_trial))
  # Get the row each item in the sequence, update Times
  trial_rows <- rows[aoi_set] %>%
    bind_rows %>%
    mutate(Time = seq_along(Time)) %>%
    as.Trial
  attributes(trial_rows) <- c(attributes(trial_rows), trial_attrs)
  trial_rows
}

# Different possibilities looks
`%rep%` <- function(x, times) rep(x, times)
aoi_sets <- list(
  targets = "Target" %rep% 10,
  nas = "NA" %rep% 10,
  stripe_1 = c("Target", "NA") %rep% 5,
  stripe_2 = c("NA", "Target") %rep% 5,
  tracked_stripe = c("tracked", "NA") %rep% 5,
  target_distractor = c("Target", "NA", "Distractor", "NA") %rep% 2,
  gap_05 = c("Target", "NA" %rep% 5, "Target"),
  gap_09 = c("Target", "NA" %rep% 9, "Target"),
  gap_10 = c("Target", "NA" %rep% 10, "Target")
)



test_that("Cases where interpolation should not occur", {

  trial <- make_trial_from_rows(aoi_sets$targets)
  expect_null(trial %@% "InterpolatedPoints")

  trial <- aoi_sets$targets %>%
    make_trial_from_rows %>%
    InterpolateMissingFrames %>%
    CalculateMistrackings
  expect_equal(trial %@% "InterpolatedPoints", 0)
  expect_equal(trial %@% "PropNA", 0)

  trial <- aoi_sets$nas %>%
    make_trial_from_rows %>%
    InterpolateMissingFrames %>%
    CalculateMistrackings
  expect_equal(trial %@% "InterpolatedPoints", 0)
  expect_equal(trial %@% "PropNA", 1)

  trial <- aoi_sets$tracked_stripe %>%
    make_trial_from_rows %>%
    InterpolateMissingFrames %>%
    CalculateMistrackings
  expect_equal(trial %@% "InterpolatedPoints", 0)
  expect_equal(trial %@% "PropNA", .5)

  trial <- aoi_sets$target_distractor %>%
    make_trial_from_rows %>%
    InterpolateMissingFrames %>%
    CalculateMistrackings
  expect_equal(trial %@% "InterpolatedPoints", 0)
  expect_equal(trial %@% "PropNA", .5)

  trial <- aoi_sets$gap_10 %>%
    make_trial_from_rows %>%
    InterpolateMissingFrames %>%
    CalculateMistrackings
  expect_equal(trial %@% "InterpolatedPoints", 0)
  expect_equal(trial %@% "PropNA", 0.833, tolerance = .001)
})



# For each trial, check that PropNA decreases
test_that("Cases where interpolation should occur", {
  trial <- aoi_sets$stripe_1 %>%
    make_trial_from_rows %>%
    CalculateMistrackings
  expect_equal(trial %@% "PropNA", .5)

  trial <- trial %>% InterpolateMissingFrames %>% CalculateMistrackings
  expect_equal(trial %@% "InterpolatedPoints", 4)
  expect_equal(trial %@% "PropNA", .1)
  expect_identical(trial$GazeByImageAOI[10], NA_character_)


  trial <- aoi_sets$stripe_2 %>%
    make_trial_from_rows %>%
    CalculateMistrackings
  expect_equal(trial %@% "PropNA", .5)

  trial <- trial %>% InterpolateMissingFrames %>% CalculateMistrackings
  expect_equal(trial %@% "InterpolatedPoints", 4)
  expect_equal(trial %@% "PropNA", .1)
  expect_identical(trial$GazeByImageAOI[1], NA_character_)


  trial <- aoi_sets$gap_05 %>%
    make_trial_from_rows %>%
    CalculateMistrackings
  expect_equal(trial %@% "PropNA", 0.714, tolerance = .001)

  trial <- trial %>% InterpolateMissingFrames %>% CalculateMistrackings
  expect_equal(trial %@% "InterpolatedPoints", 5)
  expect_equal(trial %@% "PropNA", 0)


  trial <- aoi_sets$gap_09 %>%
    make_trial_from_rows %>%
    CalculateMistrackings
  expect_equal(trial %@% "PropNA", 0.818, tolerance = .001)

  trial <- trial %>% InterpolateMissingFrames %>% CalculateMistrackings
  expect_equal(trial %@% "InterpolatedPoints", 9)
  expect_equal(trial %@% "PropNA", 0)


  # Change window to disqualify interpolation
  lwl_opts$set(interpolation_window = 100)

  trial <- aoi_sets$gap_09 %>%
    make_trial_from_rows %>%
    CalculateMistrackings %>%
    InterpolateMissingFrames
  expect_equal(trial %@% "InterpolatedPoints", 0)


  # Restore
  lwl_opts$set(interpolation_window = 150)

  trial <- aoi_sets$gap_09 %>%
    make_trial_from_rows %>%
    CalculateMistrackings %>%
    InterpolateMissingFrames
  expect_equal(trial %@% "InterpolatedPoints", 9)

})
