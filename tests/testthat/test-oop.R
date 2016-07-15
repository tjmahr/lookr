library("testthat")

context("S3 classes work as expected")

# Sessions make a Task
task_1 <- suppressMessages(Task("data/MP_NoFixations_CS1/"))
sesh_1 <- suppressMessages(Session("data/RWL_WFFArea_Long/001P00XA1/"))
sesh_2 <- suppressMessages(Session("data/RWL_WFFArea_Long/001P00XS1/"))
task_2 <- c(sesh_1, sesh_2)

# Blocks make a Sessions
blck_1 <- suppressMessages(Block("data/MP_WFFArea_Long/001P00XS1/MP_Block1_001P00XS1"))
blck_2 <- suppressMessages(Block("data/MP_WFFArea_Long/001P00XS1/MP_Block2_001P00XS1"))
sesh_3 <- c(blck_1, blck_2)

# Mismatching Tasks makes a TrialList
tlst_1 <- c(sesh_1, sesh_2, sesh_3)

test_that("Correct classes when TrialLists combined", {
  expect_is(task_1, c("Task"))
  expect_is(task_1, c("TrialList"))

  expect_is(task_2, c("Task"))
  expect_is(task_2, c("TrialList"))

  expect_is(sesh_1, c("Session"))
  expect_is(sesh_1, c("TrialList"))

  expect_is(sesh_3, c("Session"))
  expect_is(sesh_3, c("TrialList"))

  expect_is(blck_1, c("Block"))
  expect_is(blck_1, c("TrialList"))

  expect_is(tlst_1, c("TrialList"))
})


test_that("trial_lapply works and preserves classes", {
  task_1 <- AdjustTimes(task_1)
  blck_1 <- AdjustTimes(blck_1)

  expect_equal(unique(blck_1 %@% "AlignedBy"), "TargetOnset")

  expect_is(blck_1, c("Block"))
  expect_is(task_1, c("Task"))
})
