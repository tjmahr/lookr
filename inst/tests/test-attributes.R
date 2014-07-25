
library("testthat")

context("Attributes functions")

trials <- suppressMessages(Session("data/RWL_WFFArea_Long/001P00XA1/"))
trial <- trials[[1]]

test_that("Attribute getters", {
  # Correct number of items
  expect_equal(length(trial %@% "Subject"), 1)
  expect_more_than(length(trials %@% "Subject"), 1)

  # Attributes that don't exist are NULL or a list of NULLs
  expect_null(trial %@% "Visited")
  expect_null(unlist(trials %@% "Visited"))
  expect_is((trials %@% "Visited"), "list")
  expect_null((trials %@% "Visited")[[1]])
})

test_that("Attribute setters", {
  # Single item assignment
  trial %@% "Visited" <- TRUE
  expect_true(trial %@% "Visited")

  trial <- SetAttribute(trial, "Visited", FALSE)
  expect_false(trial %@% "Visited")

  # Multiple assignment for TrialLists: Infix form
  trials %@% "Visited" <- TRUE
  expect_true(all(trials %@% "Visited"))

  # Preserves classes
  expect_is(trials, "Session")

  # Multiple assignment for TrialLists: Function form
  trials <- SetAttribute(trials, "Visited", FALSE)
  expect_false(any(trials %@% "Visited"))

  # Preserves classes
  expect_is(trials, "Session")

  # No recycling needed
  expect_equivalent(trials %@% "TrialNo", 1:48)
  trials %@% "TrialNo" <- 48:1
  expect_equivalent(trials %@% "TrialNo", 48:1)

  # Neat recycling
  trials %@% "TrialNo" <- 1:24
  expect_equivalent(trials %@% "TrialNo", rep(1:24, 2))

  # Uneven recycling warns user
  expect_warning(trials %@% "TrialNo" <- 1:25)
  expect_equivalent(trials %@% "TrialNo", c(1:25, 1:23))
})


test_that("Non TrialList getter/setter behaviors", {
  Cat <- function(name, owner) {
    structure(list(Name = name), Owner = owner, class = "cat")
  }

  kiki <- Cat("Kiki", "AML")
  nooper <- Cat("Nooper", "TJM")
  cats <- list(nooper, kiki)

  # Getting is vectorized
  expect_match(kiki %@% "Owner", "AML")
  expect_equivalent(cats %@% "Owner", c("TJM", "AML"))

  # Setting/overwriting an attribute works
  cats[[1]] %@% "Owner" <- NA
  expect_equivalent(cats %@% "Owner", c(NA, "AML"))

  # Assignment is not vectorized for non-TrialLists
  cats %@% "FavoriteOwner" <- "TJM"

  expect_null(cats[[1]] %@% "FavoriteOwner")
  expect_null(unlist(cats %@% "FavoriteOwner"))

  # Instead, the attribute is given to the whole list
  expect_match(attr(cats, "FavoriteOwner"), "TJM")
})


