#' Utility Function Tests
#'
#' @description
#' Tests for choice set processing and data storage utilities
#'
#' @author Przemyslaw Marcowski, PhD <p.marcowski@gmail.com>
#' @date 2024-01-20

library(testthat)
library(mockery)

source("../../app/utils.R")

context("Utils")

# Mock data setup for testing
mock_config <- list(
  exp_id = "test_exp",
  design = list(
    n_alts = 2,
    alternatives = c("Option A", "Option B")
  ),
  ui = list(
    shuffle_attributes = FALSE
  )
)

test_that("save_experiment_data handles basic input", {
  # Create test survey data matrix
  mock_data <- list(
    survey = matrix(
      c("level1", "level2", "level1", "level2"),
      nrow = 2,
      dimnames = list(c("attr1", "attr2"), c("Option A", "Option B"))
    ),
    responses = c("Option A"),      # Single response selection
    reaction_times = c(1000)        # Response time in milliseconds
  )

  # Set storage config to NULL to test default behavior
  mock_config$storage <- NULL

  # Verify function runs without error when no storage is configured
  expect_error(save_experiment_data(mock_data, mock_config), NA)
})

test_that("custom attribute configuration is validated", {
  # Setup mock configuration for custom attributes
  mock_config <- list(
    custom_attributes = list(
      display_test = list(
        function_name = "test_func",
        attribute_label = "Test Label"
      )
    )
  )

  # Verify custom attributes validation passes without error
  expect_error(
    validate_custom_attributes(mock_config),
    NA
  )
})
