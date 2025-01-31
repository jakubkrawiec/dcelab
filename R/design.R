#' DCE Design Generation Functions
#'
#' @description
#' Core functions for generating discrete choice experiment designs
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
#' @return list containing design matrix and diagnostics
#' @export
generate_design <- function(lvls, coding, n_sets, n_alts, par_draws) {
  # Input validation
  checkmate::assert_numeric(lvls, min.len = 2)
  checkmate::assert_character(coding)
  checkmate::assert_matrix(par_draws)
  
  # Generate design using coordinate exchange algorithm
  des <- idefix::CEA(
    lvls = lvls,
    coding = coding,
    n.sets = n_sets,
    n.alts = n_alts,
    par.draws = par_draws,
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
