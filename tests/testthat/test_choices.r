#' Choice Set Generation and Processing Tests
#'
#' @description
#' Tests for choice set manipulation and presentation
#'
#' @author Przemyslaw Marcowski, PhD <p.marcowski@gmail.com>
#' @date 2024-01-20

library(testthat)
library(mockery)

source("../../app/utils.R")

context("Choice Sets")

test_that("select_choice_set handles attribute shuffling", {
  # Setup mock reactive values
  rv <- list(
    set_num = 1,
    atts_labs = list(a = c("level1", "level2"),
                     b = c("level1", "level2")),
    custom_funcs = NULL
  )

  # Create test design matrix (2x2)
  test_design <- matrix(c(0, 1, 1, 0), 2, 2)  # Use 0/1 coding

  # Define test attribute levels
  test_atts <- data.frame(
    a = c("level1", "level2"),
    b = c("level1", "level2")
  )

  # Configure test settings
  test_config <- list(
    design = list(
      n_alts = 2,
      alternatives = c("A", "B"),
      alt_cte = NULL
    ),
    ui = list(
      shuffle_attributes = TRUE
    )
  )

  # Generate choice set with test parameters
  choice_set <- select_choice_set(
    rv = rv,
    design = test_design,
    bs = c(1),
    es = c(2),
    atts = test_atts,
    atts_lvls = c(2, 2),
    atts_coding = c("D", "D"),  # Use dummy coding
    atts_num = 2,
    config = test_config
  )

  # Verify output dimensions and structure
  expect_equal(ncol(choice_set), 2)
  expect_equal(nrow(choice_set), 2)
  expect_equal(colnames(choice_set), c("A", "B"))
})
