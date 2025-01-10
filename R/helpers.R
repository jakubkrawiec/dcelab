#' Helper Functions for DCE Experiments
#'
#' @description
#' Utility functions for experiment setup and validation:
#' - Configuration validation
#'
#' @author Przemyslaw Marcowski, PhD <p.marcowski@gmail.com>
#' @date 2025-01-09

#' Validate Configuration Structure and Values
#'
#' @param config list Configuration object from yaml
#' @return NULL invisibly, errors if validation fails
#' @export
validate_config <- function(config) {
  required_fields <- list(
    "exp_id" = is.character,
    "design" = function(x) is.list(x) && all(c("n_sets", "n_total", "n_alts", "alternatives", "alt_cte") %in% names(x)),
    "ui" = function(x) is.list(x) && all(c("buttons_text", "shuffle_attributes") %in% names(x)),
    "storage" = function(x) is.list(x) && "dropbox" %in% names(x),
    "completion" = function(x) is.list(x) && "url" %in% names(x)
  )
  
  for (field in names(required_fields)) {
    if (!field %in% names(config)) {
      stop(sprintf("Missing required field: %s", field))
    }
    if (!required_fields[[field]](config[[field]])) {
      stop(sprintf("Invalid configuration for: %s", field))
    }
  }
  
  # Validate specific field types
  if (!is.logical(config$ui$shuffle_attributes)) {
    stop("ui.shuffle_attributes must be 'true' or 'false'")
  }
  
  if (!is.numeric(config$design$n_sets) || config$design$n_sets <= 0) {
    stop("design.n_sets must be a positive number")
  }
  
  if (!is.numeric(config$design$n_total) || config$design$n_total <= 0) {
    stop("design.n_total must be a positive number")
  }
  
  if (config$design$n_total > config$design$n_sets) {
    stop("design.n_total cannot be greater than design.n_sets")
  }
  
  invisible(NULL)
}
