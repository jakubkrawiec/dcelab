#' Design Generation Tests
#'
#' @description
#' Tests for D-efficient experimental design generation
#'
#' @author Przemyslaw Marcowski, PhD <p.marcowski@gmail.com>
#' @date 2024-01-20

library(testthat)
library(idefix)

source("../../R/design.R")

context("Design Generation")

test_that("generate_priors creates valid prior parameters", {
  # Create test profiles matrix
  profiles <- matrix(1:6, nrow = 2)
  # Generate priors with 5 draws
  priors <- generate_priors(profiles, n_draws = 5)

  # Verify structure and dimensions of output
  expect_type(priors, "list")
  expect_named(priors, c("mu", "sigma", "draws"))
  expect_equal(length(priors$mu), ncol(profiles))
  expect_equal(dim(priors$sigma), c(ncol(profiles), ncol(profiles)))
  expect_equal(dim(priors$draws), c(5, ncol(profiles)))
})

test_that("generate_design creates valid design matrix", {
  # Define design parameters
  lvls <- c(2, 2)               # Two attributes with 2 levels each
  coding <- c("E", "E")         # Effect coding for both attributes
  n_sets <- 4                   # Number of choice sets
  n_alts <- 2                   # Number of alternatives per set

  # Generate required inputs
  profiles <- Profiles(lvls = lvls, coding = coding)
  priors <- generate_priors(profiles, n_draws = 5)

  # Create experimental design
  design <- generate_design(
    lvls = lvls,
    coding = coding,
    n_sets = n_sets,
    n_alts = n_alts,
    par_draws = priors$draws
  )

  # Verify design structure and dimensions
  expect_type(design, "list")
  expect_true("design" %in% names(design))
  expect_true("error" %in% names(design))
  expect_equal(nrow(design$design), n_sets * n_alts)
  expect_equal(ncol(design$design), length(lvls))

  # Check validity of design matrix values
  design_matrix <- design$design
  for (col in 1:ncol(design_matrix)) {
    unique_values <- unique(design_matrix[, col])
    expect_lte(length(unique_values), lvls[col])
    expect_true(all(is.finite(unique_values)))
  }
})

test_that("generate_design handles input validation", {
  # Test error handling with invalid input
  expect_error(
    generate_design(
      lvls = "invalid",
      coding = c("E"),
      n_sets = 10,
      n_alts = 2,
      par_draws = matrix(1:6, nrow = 2)
    ),
    "Assertion on 'lvls' failed"
  )
})

test_that("generate_design processes effect coding correctly", {
  # Define design parameters
  lvls <- c(2, 2)
  coding <- c("E", "E")
  n_sets <- 4
  n_alts <- 2

  # Generate design inputs
  profiles <- Profiles(lvls = lvls, coding = coding)
  priors <- generate_priors(profiles, n_draws = 5)

  # Create experimental design
  design <- generate_design(
    lvls = lvls,
    coding = coding,
    n_sets = n_sets,
    n_alts = n_alts,
    par_draws = priors$draws
  )

  # Verify effect coding (-1, 1) is properly applied
  design_matrix <- design$design
  for (col in 1:ncol(design_matrix)) {
    unique_values <- sort(unique(design_matrix[,col]))
    expect_true(all(unique_values %in% c(-1, 1)) ||
                all(abs(unique_values) <= 1))
  }
})
