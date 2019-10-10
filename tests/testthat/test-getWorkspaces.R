context("Get Workspaces")
library(testthat)
library(Noir)

test_that("getWorkspaces() will give an output message when not connected to API", {
  expect_output(getWorkspaces(urlWSP), "Get Workspaces Generated an Error")
})