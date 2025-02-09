#' DCE Testing Configuration
#'
#' @description
#' Main test configuration and runner for DCE experiment tests
#'
#' @author Przemyslaw Marcowski, PhD <p.marcowski@gmail.com>
#' @date 2024-01-20

library(testthat)
library(yaml)
library(idefix)
library(mockery)

source("R/design.R")
source("R/helpers.R")
source("app/utils.R")

# Set test reporter
reporter <- MultiReporter$new(list(
  ProgressReporter$new(),
  JunitReporter$new(file = "test-results.xml")
))

# Run tests
test_dir(
  "tests/testthat",
  reporter = reporter,
  stop_on_failure = TRUE,
  parallel = TRUE
)
