#' Custom Attribute Processing Tests
#'
#' @description
#' Tests for custom attribute function handling
#'
#' @author Przemyslaw Marcowski, PhD <p.marcowski@gmail.com>
#' @date 2024-01-20

library(testthat)
library(mockery)

source("../../app/utils.R")

context("Custom Attributes")

test_that("apply_custom_attributes processes functions correctly", {
  # Create sample choice set matrix with two levels
  choice_set <- matrix(
    c("level1", "level2"),
    nrow = 1,
    dimnames = list("attr1", c("A", "B"))
  )

  # Set up mock environment with test function
  mock_env <- new.env()
  mock_env$test_func <- function(context) {
    # Generate processed values based on column index
    setNames(paste("processed", context$col_index),
            context$config$design$alternatives[context$col_index])
  }

  # Define custom attribute functions list
  custom_funcs <- list(
    test_attr = list(
      func = mock_env$test_func,
      label = "Test Attribute",
      function_name = "test_func"
    )
  )

  # Create configuration with alternative names
  config <- list(
    design = list(
      alternatives = c("A", "B")
    )
  )

  # Apply custom attributes to choice set
  result <- apply_custom_attributes(choice_set, custom_funcs, config)

  # Verify result structure and content
  expect_equal(nrow(result), 2)
  expect_true("Test Attribute" %in% rownames(result))
  expect_equal(unname(result["Test Attribute", ]), c("processed 1", "processed 2"))
})

test_that("load_custom_functions validates function parameters", {
  # Create temporary directory and custom.R file
  temp_dir <- tempdir()
  custom_file <- file.path(temp_dir, "custom.R")

  # Write test function to temporary custom.R
  writeLines('
    invalid_func <- function(x) x
  ', custom_file)

  # Define test configuration
  mock_config <- list(
    custom_attributes = list(
      test = list(
        function_name = "invalid_func",
        attribute_label = "Test"
      )
    )
  )

  # Verify error message about missing context parameter
  expect_error(
    load_custom_functions(mock_config, temp_dir),
    "Function 'invalid_func' must have parameter: context"
  )

  # Clean up temporary file
  unlink(custom_file)
})
