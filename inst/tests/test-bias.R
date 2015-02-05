context("Bias calculation")

# Create an 8-frame Trial for testing bias calculations
DummyTrial <- function(...) {
  # Make sure negative times are included.
  times <- seq(-3, 4) * lwl_constants$ms_per_frame
  # Convert the numbers in the dots to AOI values
  names <- c("Target", "SemanticFoil", "tracked", NA)
  aois <- names[c(...)]
  # Package as a trial
  trial <- data.frame(Time = times, GazeByImageAOI = aois,
                      row.names = NULL, stringsAsFactors = FALSE)
  trial$Subject <- "001L"
  structure(as.Trial(trial), Subject = "001L", Task = "RWL")
}

test_that("Bias assigned to most viewed AOI", {
  # Trials where the clear winner is the Target and it's viewed first or second
  leading <- DummyTrial(1, 1, 1, 1, 3, 2, 2, 2)
  lagging <- DummyTrial(2, 2, 2, 3, 1, 1, 1, 1)
  middled <- DummyTrial(2, 2, 1, 1, 1, 4, 4, 4)

  window <- c(-50, 75)
  leading <- CalculateBias(leading, window)
  lagging <- CalculateBias(lagging, window)
  middled <- CalculateBias(middled, window)

  expect_equal(leading %@% "Bias", "Target")
  expect_equal(lagging %@% "Bias", "Target")
  expect_equal(middled %@% "Bias", "Target")

  # Confirm invalid AOIs removed from summary
  expect_equal(nrow(leading %@% "BiasSummary"), 2)
})


test_that("Bias breaks ties with first image", {
  # Trials where looks to Target and Semantic are tied but Target is always
  # viewed first.
  tied1 <- DummyTrial(1, 1, 2, 2, 3, 3, 4, 4)
  tied2 <- DummyTrial(1, 1, 4, 3, 3, 4, 2, 2)
  tied3 <- DummyTrial(1, 2, 3, 4, 1, 2, 3, 4)

  window <- c(-50, 75)
  tied1 <- CalculateBias(tied1, window)
  tied2 <- CalculateBias(tied2, window)
  tied3 <- CalculateBias(tied3, window)

  expect_equal(tied1 %@% "Bias", "Target")
  expect_equal(tied2 %@% "Bias", "Target")
  expect_equal(tied3 %@% "Bias", "Target")


})

test_that("NA bias values", {
  # Trial where there is no valid winner
  no_aois <- DummyTrial(4, 4, 4, 4, 3, 3, 4, 4)
  window <- c(-50, 75)
  no_aois <- CalculateBias(no_aois, window)

  # No bias, no rows in summary
  expect_equal(no_aois %@% "Bias", NA_character_)
  expect_equal(nrow(no_aois %@% "BiasSummary"), 0)
})


