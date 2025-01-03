#' DCE Design Generation Functions
#'
#' @description
#' Core functions for generating and evaluating discrete choice experiment designs:
#' - Design generation using coordinate exchange algorithm
#' - Prior parameter generation for Bayesian D-optimal designs 
#' - Design evaluation metrics and balance checking
#'
#' @author Przemyslaw Marcowski, PhD <p.marcowski@gmail.com>
#' @date 2024-12-24

#' Generate Experimental Design
#'
#' @param lvls numeric vector of attribute levels
#' @param coding character vector of coding types ('E', 'D', 'C')
#' @param n_sets integer number of choice sets
#' @param n_alts integer number of alternatives
#' @param par_draws matrix of parameter draws
#' @param alt_cte logical vector alternative-specific constants
#' @param parallel logical use parallel processing
#' @return list containing design matrix and diagnostics
#' @export
generate_design <- function(lvls, coding, n_sets, n_alts, par_draws, 
                            alt_cte = NULL, parallel = FALSE) {
  # Input validation
  checkmate::assert_numeric(lvls, min.len = 2)
  checkmate::assert_character(coding)
  checkmate::assert_matrix(par_draws)
  
  if (is.null(alt_cte)) {
    alt_cte <- rep(0L, n_alts)
  }
  
  # Generate design using coordinate exchange algorithm
  des <- idefix::CEA(
    lvls = lvls,
    coding = coding,
    n.sets = n_sets,
    n.alts = n_alts,
    par.draws = par_draws,
    alt.cte = alt_cte,
    parallel = parallel,
    best = TRUE
  )
  
  return(des)
}

#' Generate Prior Parameters
#'
#' @param profiles matrix candidate profiles
#' @param n_draws integer number of draws
#' @return list containing mu, sigma, and parameter draws
#' @export
generate_priors <- function(profiles, n_draws = 10) {
  # Input validation
  checkmate::assert_matrix(profiles)
  checkmate::assert_count(n_draws)
  
  # Generate naive priors
  mu <- rep(0, ncol(profiles))
  sigma <- diag(length(mu))
  
  # Generate parameter draws
  par_draws <- MASS::mvrnorm(n_draws, mu, sigma)
  
  list(
    mu = mu,
    sigma = sigma,
    draws = par_draws
  )
}

#' Evaluate Design Balance
#'
#' @param design matrix design matrix
#' @param n_alts integer number of alternatives
#' @param lvl_names list level names
#' @param coding character vector coding types
#' @return list containing level balance and decoded design
#' @export
evaluate_design <- function(design, n_alts, lvl_names, coding) {
  # Input validation
  checkmate::assert_matrix(design)
  checkmate::assert_count(n_alts)
  checkmate::assert_list(lvl_names)
  checkmate::assert_character(coding)
  
  # Decode design
  decoded <- idefix::Decode(
    des = design,
    n.alts = n_alts,
    lvl.names = lvl_names,
    coding = coding
  )
  
  # Return evaluation results
  list(
    level_balance = decoded$lvl.balance,
    design = decoded$design
  )
}

#' Create Row and Column Names for Design Matrix
#'
#' @param n_sets integer number of choice sets
#' @param n_alts integer number of alternatives
#' @param alt_cte logical vector alternative-specific constants
#' @return list containing row and column names
#' @export
create_design_names <- function(n_sets, n_alts, alt_cte) {
  # Input validation
  checkmate::assert_count(n_sets)
  checkmate::assert_count(n_alts)
  checkmate::assert_logical(alt_cte)
  
  # Create row names
  row_names <- paste0(
    rep(paste0("set", 1:n_sets), each = n_alts),
    "_alt",
    rep(1:n_alts, times = n_sets)
  )
  
  # Create column names for alternative-specific constants
  col_names <- if (any(alt_cte == 1)) {
    paste0("alt", which(alt_cte == 1), "_cte")
  } else NULL
  
  list(
    row_names = row_names,
    col_names = col_names
  )
}

#' Calculate D-error
#'
#' @param des list design object
#' @param par_draws matrix parameter draws
#' @param n_alts integer number of alternatives
#' @return numeric D-error value
#' @export
calculate_d_error <- function(des, par_draws, n_alts) {
  # Input validation
  checkmate::assert_list(des)
  checkmate::assert_matrix(par_draws)
  checkmate::assert_count(n_alts)
  
  # Calculate D-error for each parameter draw
  d_errors <- apply(par_draws, 1, function(par) {
    info_mat <- idefix::InfoDes_cpp(par, des$design, n_alts)
    det <- idefix::det_cpp(info_mat)
    
    if (is.nan(det) || det <= 0) {
      NA
    } else {
      det^(-1/length(par))
    }
  })
  
  # Return mean D-error
  mean(d_errors, na.rm = TRUE)
}

#' Check Design Balance
#'
#' @param design matrix design matrix
#' @param lvls numeric vector attribute levels
#' @return data.frame balance metrics
#' @export
check_design_balance <- function(design, lvls) {
  # Input validation
  checkmate::assert_matrix(design)
  checkmate::assert_numeric(lvls)
  
  # Calculate frequency tables for each attribute
  balance <- lapply(1:length(lvls), function(i) {
    freq <- table(design[, i])
    data.frame(
      attribute = i,
      level = names(freq),
      frequency = as.numeric(freq),
      proportion = as.numeric(prop.table(freq))
    )
  })
  
  # Combine results
  do.call(rbind, balance)
}
