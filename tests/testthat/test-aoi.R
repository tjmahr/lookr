context("AOI mappings")

filter_out_targets <- MakeAttributeFilter(attr_name = "TargetImage")

test_that("Two Image Task: Left < .5 < Right", {
  mp_trials <- suppressMessages(Session("data/MP_WFFArea_Long/001P00XS1/"))
  mp_trials <- AddRelativeGazes(mp_trials)
  mp_trials <- AddAOIData(mp_trials)

  # Get the second (i.e., first non-practice) "ImageL" and "ImageR" trials
  right_trial <- filter_out_targets(mp_trials, "ImageL")[[2]]
  left_trial <- filter_out_targets(mp_trials, "ImageR")[[2]]
  two_img <- rbind.fill(left_trial, right_trial)

  # Keep just looks inside the two regions
  two_img <- two_img[c("XMean", "XMeanToTarget", "GazeByAOI", "GazeByImageAOI")]
  two_img <- two_img[two_img$GazeByAOI != "tracked", ]
  two_img <- na.omit(two_img)

  # Make sure there are looks to 2 image-regions x 2 target-location conditions
  combinations <- unique(two_img[c("GazeByImageAOI", "GazeByAOI")])
  expect_equal(nrow(combinations), 4)

  img_L <- str_detect(two_img$GazeByAOI, "ImageL")
  img_R <- str_detect(two_img$GazeByAOI, "ImageR")
  lefts  <- two_img[img_L, "XMean"]
  rights <- two_img[img_R, "XMean"]

  aoi_t <- str_detect(two_img$GazeByImageAOI, "Target")
  aoi_d <- str_detect(two_img$GazeByImageAOI, "Distractor")
  targets <- two_img[aoi_t, "XMeanToTarget"]
  distractors <- two_img[aoi_d, "XMeanToTarget"]

  # Correct XMean locations
  expect_true(all(lefts < .5))
  expect_true(all(.5 < rights))

  # Correct XMeanToTarget locations
  expect_true(all(distractors < .5))
  expect_true(all(.5 < targets))
})


test_that("Four Image Task: Left < .5 < Right, Down < .5 < Up", {
  four_places <- c("UpperLeftImage", "LowerLeftImage",
                   "UpperRightImage", "LowerRightImage")

  has_looks_in_four_regions <- function(trial) {
    all(is.element(four_places, trial$GazeByAOI))
  }

  # Keep just trials where all four images are viewed
  rwl_trials <- suppressMessages(Session("data/RWL_NoFixations_CS1/001P00XA1/"))
  rwl_trials <- AddAOIData(rwl_trials)
  rwl_trials <- as.TrialList(Filter(has_looks_in_four_regions, rwl_trials))

  # Keep one trial from each of the four possible target locations
  t1 <- filter_out_targets(rwl_trials, four_places[-1])[[1]]
  t2 <- filter_out_targets(rwl_trials, four_places[-2])[[1]]
  t3 <- filter_out_targets(rwl_trials, four_places[-3])[[1]]
  t4 <- filter_out_targets(rwl_trials, four_places[-4])[[1]]
  four_img <- rbind.fill(t1, t2, t3, t4)

  # Filter out NA or between-image looks
  four_img <- four_img[c("XMean", "YMean", "GazeByAOI", "GazeByImageAOI")]
  four_img <- four_img[four_img$GazeByAOI != "tracked", ]
  four_img <- na.omit(four_img)

  # Make sure there are looks with the target in each location
  looking_to_target <- four_img[four_img$GazeByImageAOI == "Target", ]
  combinations <- unique(looking_to_target[c("GazeByAOI", "GazeByImageAOI")])
  expect_equal(nrow(combinations), 4)

  # Keep just x value for left-right contrast, y value for up-down contrast
  four_img_detect <- function(pattern) str_detect(four_img$GazeByAOI, pattern)
  lefts <- four_img[four_img_detect("Left"), "XMean"]
  rights <- four_img[four_img_detect("Right"), "XMean"]
  lowers <- four_img[four_img_detect("Lower"), "YMean"]
  uppers <- four_img[four_img_detect("Upper"), "YMean"]

  # Location with respect to middle of screen
  expect_true(all(lowers < .5))
  expect_true(all(.5 < uppers))
  expect_true(all(lefts < .5))
  expect_true(all(.5 < rights))
})
