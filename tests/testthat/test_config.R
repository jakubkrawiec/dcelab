#' Configuration Validation Tests
#'
#' @description
#' Tests for experiment configuration validation functions
#'
#' @author Przemyslaw Marcowski, PhD <p.marcowski@gmail.com>
#' @date 2024-01-20

library(testthat)
library(mockery)

source("../../R/helpers.R")

context("Configuration Validation")

# Define a valid configuration for testing purposes
valid_config <- list(
  exp_id = "test_experiment",
  design = list(
    n_sets = 30,                                    # Number of choice sets
    n_total = 30,                                   # Total number of sets to show
    n_alts = 2,                                     # Number of alternatives per set
    alternatives = c("Option A", "Option B")        # Names of alternatives
  ),
  ui = list(
    buttons_text = "Which option do you prefer?",   # Question text
    shuffle_attributes = FALSE,                     # Whether to randomize attribute order
    default_option = "random",                      # Default selection behavior
    no_choice = "Don't know",                       # Text for no-choice option
    explicit_choice = TRUE                          # Require explicit selection
  ),
  storage = list(
    s3 = list(
      bucket = "test-bucket",                       # S3 bucket name
      prefix = "data/raw",                          # Path prefix for storage
      region = "test-region",                       # AWS region
      access_key = "test-key",                      # AWS access key
      secret_key = "test-secret"                    # AWS secret key
    )
  )
)

# Valid configuration should not raise errors
test_that("validate_config accepts valid configuration", {
  expect_silent(validate_config(valid_config))
})

# Required fields validation
test_that("validate_config checks required fields", {
  invalid_config <- valid_config
  invalid_config$exp_id <- NULL
  expect_error(validate_config(invalid_config), "Missing required field: exp_id")
})

# Design parameters validation
test_that("validate_config validates design parameters", {
  # Check for negative number of sets
  invalid_config <- valid_config
  invalid_config$design$n_sets <- -1
  expect_error(validate_config(invalid_config), "design.n_sets must be a positive number")

  # Check total sets don't exceed available sets
  invalid_config <- valid_config
  invalid_config$design$n_total <- 31
  expect_error(validate_config(invalid_config),
               "design.n_total cannot be greater than design.n_sets")
})

# UI configuration validation
test_that("validate_config validates UI configuration", {
  # Verify that invalid shuffle_attributes value is rejected
  invalid_config <- valid_config
  invalid_config$ui$shuffle_attributes <- "invalid_mode"
  expect_error(validate_config(invalid_config),
               "ui.shuffle_attributes must be FALSE, 'trial', or 'participant'")
  
  # Check shuffle_attributes = "trial"
  valid_config_trial <- valid_config
  valid_config_trial$ui$shuffle_attributes <- "trial"
  expect_silent(validate_config(valid_config_trial))
  
  # Check shuffle_attributes = "participant"
  valid_config_participant <- valid_config
  valid_config_participant$ui$shuffle_attributes <- "participant"
  expect_silent(validate_config(valid_config_participant))
  
  # Check shuffle_attributes = FALSE
  valid_config_false <- valid_config
  valid_config_false$ui$shuffle_attributes <- FALSE
  expect_silent(validate_config(valid_config_false))
  
  # Check if default_option validation works
  invalid_config <- valid_config
  invalid_config$ui$default_option <- "Option C"
  expect_error(validate_config(invalid_config),
               "ui.default_option must be null, 'random', or one of:")
})
