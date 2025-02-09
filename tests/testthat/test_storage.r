#' Storage Configuration and Data Saving Tests
#'
#' @description
#' Tests for data storage functionality and S3 integration
#'
#' @author Przemyslaw Marcowski, PhD <p.marcowski@gmail.com>
#' @date 2024-01-20

library(testthat)
library(mockery)
library(aws.s3)

source("../../app/utils.R")

context("Storage")

test_that("save_to_s3 handles file operations correctly", {
  # Create sample test data
  test_data <- data.frame(
    set = 1:2,
    response = c("Option A", "Option B")
  )

  # Define mock S3 configuration parameters
  s3_config <- list(
    bucket = "test-bucket",
    prefix = "data/raw",
    region = "us-east-1",
    access_key = "test-key",
    secret_key = "test-secret"
  )

  # Mock the S3 put_object function
  mock_put_object <- mock(TRUE)
  stub(save_to_s3, "aws.s3::put_object", mock_put_object)

  # Call the function under test
  save_to_s3(test_data, "test.txt", s3_config, "test_exp")

  # Verify mocked S3 operation was called once
  expect_called(mock_put_object, 1)

  # Check if S3 parameters were passed correctly
  mock_call <- mock_args(mock_put_object)[[1]]
  expect_equal(mock_call$bucket, "test-bucket")
  expect_equal(mock_call$object, "data/raw/test_exp/test.txt")
})

test_that("save_experiment_data handles missing storage configuration", {
  # Create test matrix with named dimensions
  test_data <- list(
    survey = matrix(
      c("level1", "level2", "level1", "level2"),
      nrow = 2,
      dimnames = list(c("attr1", "attr2"), c("Option A", "Option B"))
    ),
    responses = c("Option A", "Option B"),
    reaction_times = c(1000, 2000)
  )

  # Minimal configuration without storage settings
  test_config <- list(exp_id = "test")

  # Verify function runs without errors when storage config is missing
  expect_silent(save_experiment_data(test_data, test_config))
})

test_that("save_experiment_data formats data correctly", {
  # Create test matrix with response data
  test_data <- list(
    survey = matrix(
      c("level1", "level2", "level1", "level2"),
      nrow = 2,
      dimnames = list(c("attr1", "attr2"), c("Option A", "Option B"))
    ),
    responses = c("Option A", "Option B"),
    reaction_times = c(1000, 2000)
  )

  # Configuration with S3 storage parameters
  test_config <- list(
    exp_id = "test",
    storage = list(
      s3 = list(
        bucket = "test-bucket",
        prefix = "data/raw",
        region = "test-region",
        access_key = "test-key",
        secret_key = "test-secret"
      )
    )
  )

  # Mock the S3 storage function
  mock_s3 <- mock(TRUE)
  stub(save_experiment_data, "save_to_s3", mock_s3)

  # Execute the function under test
  save_experiment_data(test_data, test_config)

  # Verify S3 save was called exactly once
  expect_called(mock_s3, 1)
})
