context("AOI mappings")

rwl_trials <- suppressMessages(Session("data/RWL_NoFixations_CS1/001P00XA1/"))
rwl_trials <- AddAOIData(rwl_trials)
trial <- rwl_trials[[8]]

# Keep just looks to the four images
locations <- trial[c("XMean", "YMean", "GazeByAOI")]
locations <- locations[locations$GazeByAOI != "tracked", ]
locations <- na.omit(locations)

# Extract location information
up_down <- ifelse(str_detect(locations$GazeByAOI, "Upper"), "Upper", "Lower")
left_right <- ifelse(str_detect(locations$GazeByAOI, "Left"), "Left", "Right")
locations$Vertical <- up_down
locations$Horizontal <- left_right

lowers <- locations[locations$Vertical == "Lower", "YMean"]
uppers <- locations[locations$Vertical == "Upper", "YMean"]
lefts <- locations[locations$Horizontal == "Left", "XMean"]
rights <- locations[locations$Horizontal == "Right", "XMean"]

test_that("Left is smaller than Right, Down is smaller than Up", {
  # Correct number of items
  expect_equal(length(unique(locations$GazeByAOI)), 4)

  expect_true(all(lowers[1:100] < uppers[1:100]))
  expect_true(all(lefts[1:100] < rights[1:100]))
})





