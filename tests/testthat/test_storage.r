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

test_that("save_experiment_data structures data correctly for different configurations", {
  # Create test matrix with meaningful attribute structure
  base_matrix <- matrix(
    c("10%", "20%", "30%",
      "$100", "$200", "$300",
      "Yes", "No", "Maybe"),
    nrow = 3,
    dimnames = list(
      c("Discount", "Price", "Available"),
      c("Option A", "Option B", "Option C")
    )
  )
  
  # Test standard configuration
  standard_data <- list(
    survey = base_matrix[, 1:2],  # Only use first two columns for responses
    responses = c("Option A", "Option B"),
    reaction_times = c(1500, 2000)
  )
  standard_config <- list(exp_id = "test")
  
  formatted_data <- save_experiment_data(standard_data, standard_config)
  expect_equal(nrow(formatted_data), nrow(base_matrix) * length(standard_data$responses))
  expect_equal(sort(unique(formatted_data$attribute)), sort(rownames(base_matrix)))
  expect_equal(sort(unique(formatted_data$response)), sort(standard_data$responses))
  
  # Test participant-level attribute shuffling
  shuffled_order <- c("Price", "Available", "Discount")
  shuffled_matrix <- base_matrix[shuffled_order, 1:2]
  participant_data <- list(
    survey = shuffled_matrix,
    responses = c("Option B", "Option A"),
    reaction_times = c(1200, 1800)
  )
  participant_config <- list(
    exp_id = "test",
    ui = list(shuffle_attributes = "participant")
  )
  
  formatted_data <- save_experiment_data(participant_data, participant_config)
  expect_equal(sort(unique(formatted_data$attribute)), sort(shuffled_order))
  
  # Test with no-choice option
  nochoice_data <- list(
    survey = base_matrix[, 1:2],
    responses = c("Option A", "Don't know"),
    reaction_times = c(1000, 3000)
  )
  nochoice_config <- list(
    exp_id = "test",
    ui = list(no_choice = "Don't know")
  )
  
  formatted_data <- save_experiment_data(nochoice_data, nochoice_config)
  expect_true("Don't know" %in% formatted_data$response)
  expect_equal(sort(unique(formatted_data$response)), sort(c("Option A", "Don't know")))
  
  # Test with default options
  default_data <- list(
    survey = base_matrix[, 1:2],
    responses = c("Option C", "Option A"),
    reaction_times = c(800, 1200),
    defaults = c("Option C", "Option B")
  )
  default_config <- list(
    exp_id = "test",
    ui = list(default_option = "random")
  )
  
  formatted_data <- save_experiment_data(default_data, default_config)
  expect_true("default" %in% colnames(formatted_data))
  expect_equal(sort(unique(formatted_data$default)), sort(default_data$defaults))
  expect_equal(sort(unique(formatted_data$reaction_time)), sort(default_data$reaction_times))
})
