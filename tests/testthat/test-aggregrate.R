context("Aggregating looking data")

library("plyr")

# Create a dummy trial
make_trial <- function(trial_no, aois) {
  times <- seq_along(aois)
  data.frame(
    Time = times,
    TrialNo = trial_no,
    GazeByImageAOI = aois,
    stringsAsFactors = FALSE)
}

`%rep%` <- function(x, times) rep(x, times)

# Different possibilities of 10 looks
aoi_sets <- list(
  targets = "Target" %rep% 10,
  nas = NA %rep% 10,
  spotty = c(NA, "Target", "tracked") %rep% c(4, 3, 3),
  mixed = c("Target", "tracked", "Foil1", NA) %rep% 4:1,
  two_aois = c("Target", "Foil1") %rep% 5,
  multiple_aois = c("Target", "Foil1", "Foil2", "Foil3") %rep% 4:1
)

# Create trials
trial_list <- Map(make_trial, 1:6, aoi_sets)
four_aois <- ldply(trial_list)
four_aois$Subject <- "001X"

# Simulate a two-image experiment by removing the trial where the other 2 images
# are fixated
two_aois <- subset(four_aois, TrialNo != 6)

test_that("AOI names become column names", {
  # Perform aggregrations
  two_aoi_agg <- AggregateLooks(two_aois, Subject + TrialNo ~ GazeByImageAOI)
  four_aoi_agg <- AggregateLooks(four_aois, Subject + TrialNo ~ GazeByImageAOI)

  # Names not related to the distractor images
  exp_names <- c("Target", "Elsewhere", "Others", "NAs",
                 "Proportion", "ProportionSE", "Looks", "PropNA")
  aggregating_vars <- c("Subject", "TrialNo")

  # Add names related to the distractor images
  two_aoi_expected <- c(exp_names, aggregating_vars, "Foil1")
  four_aoi_expected <- c(exp_names, aggregating_vars, "Foil1", "Foil2", "Foil3")

  name_diff_1 <- setdiff(names(two_aoi_agg), two_aoi_expected)
  name_diff_2 <- setdiff(names(four_aoi_agg), four_aoi_expected)

  expect_equal(length(name_diff_1), 0)
  expect_equal(length(name_diff_2), 0)

})


test_that("Proportions make sense", {
  # Perform aggregrations
  two_aoi_agg <- AggregateLooks(two_aois, Subject + TrialNo ~ GazeByImageAOI)
  rownames(two_aoi_agg) <- names(aoi_sets)[1:5]

  four_aoi_agg <- AggregateLooks(four_aois, Subject + TrialNo ~ GazeByImageAOI)
  rownames(four_aoi_agg) <- names(aoi_sets)

  # No data case
  expect_true(is.nan(two_aoi_agg["nas", "Proportion"]))
  expect_equal(two_aoi_agg["nas", "PropNA"], 1)

  # Check specific values
  expect_equal(two_aoi_agg["two_aois", "Looks"], 10)
  expect_equal(two_aoi_agg["spotty", "PropNA"], .4)
  expect_equal(two_aoi_agg["targets", "PropNA"], 0)
  expect_equal(two_aoi_agg["targets", "Proportion"], 1)
  expect_equal(two_aoi_agg["two_aois", "Proportion"], .5)

  expect_equal(four_aoi_agg["two_aois", "Proportion"], .5)
  expect_equal(four_aoi_agg["multiple_aois", "Proportion"], .4)

  # Aggregate again, ignore trial numbers
  two_aoi_agg <- AggregateLooks(two_aois, Subject ~ GazeByImageAOI)

  to_targ <- sum(two_aois$GazeByImageAOI == "Target", na.rm = TRUE)
  to_foil <- sum(two_aois$GazeByImageAOI == "Foil1", na.rm = TRUE)
  to_na <- sum(is.na(two_aois$GazeByImageAOI))

  expect_equal(two_aoi_agg$Proportion, to_targ / (to_foil + to_targ))
  expect_equal(two_aoi_agg$NAs, to_na)
  expect_equal(two_aoi_agg$PropNA, to_na / 50)

})
