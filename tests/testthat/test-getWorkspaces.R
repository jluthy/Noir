context("Get Workspaces")
library(testthat)
library(Noir)

test_that("getWorkspaces() will give an output message when not connected to API", {
  expect_output(getWorkspaces(url), "Get Workspaces Generated an Error")
})