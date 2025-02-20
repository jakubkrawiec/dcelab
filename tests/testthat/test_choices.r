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

test_that("select_choice_set handles different attribute shuffling modes", {
  # Setup mock environment with reactive values
  rv <- new.env()
  rv$set_num <- 1
  rv$atts_labs <- list(a = c("level1", "level2"),
                       b = c("level1", "level2"))
  rv$custom_funcs <- NULL
  rv$attribute_order <- NULL
  
  # Create test design matrix (2x2)
  test_design <- matrix(rep(c(0, 1, 1, 0), 5), 10, 2) # Use 0/1 coding
  
  bs <- seq(1, nrow(test_design), 2)
  es <- bs + 1
  
  # Define test attribute levels
  test_atts <- data.frame(
    a = c("level1", "level2"),
    b = c("level1", "level2")
  )
  
  # Configure test settings
  base_config <- list(
    design = list(
      n_alts = 2,
      alternatives = c("A", "B"),
      alt_cte = NULL
    ),
    ui = list()
  )
  
  # Test participant-level shuffling
  participant_config <- base_config
  participant_config$ui$shuffle_attributes <- "participant"
  
  choice_set1 <- select_choice_set(
    rv = rv,
    design = test_design,
    bs = bs,
    es = es,
    atts = test_atts,
    atts_lvls = c(2, 2),
    atts_coding = c("D", "D"),
    atts_num = 2,
    config = participant_config
  )
  
  # Save first choice set order
  first_order <- rownames(choice_set1)
  
  # Generate second choice set
  rv$set_num <- 2
  choice_set2 <- select_choice_set(
    rv = rv,
    design = test_design,
    bs = bs,
    es = es,
    atts = test_atts,
    atts_lvls = c(2, 2),
    atts_coding = c("D", "D"),
    atts_num = 2,
    config = participant_config
  )
  
  # Verify participant-level consistency
  expect_equal(rownames(choice_set2), first_order)
  
  # Test trial-level shuffling
  trial_config <- base_config
  trial_config$ui$shuffle_attributes <- "trial"
  rv$set_num <- 1
  rv$attribute_order <- NULL
  
  choice_sets <- replicate(10, {
    select_choice_set(
      rv = rv,
      design = test_design,
      bs = bs,
      es = es,
      atts = test_atts,
      atts_lvls = c(2, 2),
      atts_coding = c("D", "D"),
      atts_num = 2,
      config = trial_config
    )
  }, simplify = FALSE)
  
  orders <- lapply(choice_sets, rownames)
  expect_true(length(unique(orders)) > 1)
  
  # Test no shuffling
  no_shuffle_config <- base_config
  no_shuffle_config$ui$shuffle_attributes <- FALSE
  rv$attribute_order <- NULL
  
  no_shuffle_set <- select_choice_set(
    rv = rv,
    design = test_design,
    bs = bs,
    es = es,
    atts = test_atts,
    atts_lvls = c(2, 2),
    atts_coding = c("D", "D"),
    atts_num = 2,
    config = no_shuffle_config
  )
  
  expect_equal(rownames(no_shuffle_set), names(test_atts))
})
