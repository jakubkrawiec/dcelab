#' Helper Functions for DCE Experiments
#'
#' @description
#' Utility functions for experiment setup and validation
#'
#' @author Przemyslaw Marcowski, PhD <p.marcowski@gmail.com>
#' @date 2025-01-20

#' Validate Configuration Structure and Values
#'
#' @param config list Configuration object from yaml
#' @return NULL invisibly, errors if validation fails
#' @export
validate_config <- function(config) {
  # Define required fields and their validation functions
  required_fields <- list(
    "exp_id" = is.character,
    "design" = function(x) is.list(x) && all(c("n_sets", "n_total", "n_alts", "alternatives", "alt_cte") %in% names(x)),
    "ui" = function(x) is.list(x) && all(c("buttons_text", "shuffle_attributes") %in% names(x)),
    "storage" = function(x) is.list(x) && "dropbox" %in% names(x),
    "completion" = function(x) is.list(x) && "url" %in% names(x)
  )
  
  # Check each required field exists and is valid
  for (field in names(required_fields)) {
    if (!field %in% names(config)) {
      stop(sprintf("Missing required field: %s", field))
    }
    if (!required_fields[[field]](config[[field]])) {
      stop(sprintf("Invalid configuration for: %s", field))
    }
  }
  
  # Validate UI configuration
  if (!is.logical(config$ui$shuffle_attributes)) {
    stop("ui.shuffle_attributes must be 'true' or 'false'")
  }
  
  # Validate design parameters
  if (!is.numeric(config$design$n_sets) || config$design$n_sets <= 0) {
    stop("design.n_sets must be a positive number")
  }
  
  if (!is.numeric(config$design$n_total) || config$design$n_total <= 0) {
    stop("design.n_total must be a positive number")
  }
  
  if (config$design$n_total > config$design$n_sets) {
    stop("design.n_total cannot be greater than design.n_sets")
  }
  
  # Validate custom attributes if present
  if (!is.null(config$custom_attributes)) {
    validate_custom_attributes(config)
  }
  
  invisible(NULL)
}

#' Validate Custom Attributes Configuration
#'
#' @param config list Configuration object
#' @return NULL invisibly, errors if validation fails
#' @export
validate_custom_attributes <- function(config) {
  # Ensure custom_attributes is a list
  if (!is.list(config$custom_attributes)) {
    stop("custom_attributes must be a list")
  }
  
  # Validate each custom attribute configuration
  for (attr_name in names(config$custom_attributes)) {
    attr_config <- config$custom_attributes[[attr_name]]
    
    # Check attribute configuration structure
    if (!is.list(attr_config)) {
      stop(sprintf("Configuration for '%s' must be a list", attr_name))
    }
    
    # Verify required fields presence
    required_fields <- c("function_name", "attribute_label")
    missing_fields <- setdiff(required_fields, names(attr_config))
    
    if (length(missing_fields) > 0) {
      stop(sprintf(
        "Missing required fields for custom attribute '%s': %s",
        attr_name,
        paste(missing_fields, collapse = ", ")
      ))
    }
    
    # Validate field types and lengths
    if (!is.character(attr_config$function_name) || length(attr_config$function_name) != 1) {
      stop(sprintf("'function_name' for '%s' must be a single string", attr_name))
    }
    
    if (!is.character(attr_config$attribute_label) || length(attr_config$attribute_label) != 1) {
      stop(sprintf("'attribute_label' for '%s' must be a single string", attr_name))
    }
  }
  
  invisible(NULL)
}

#' Load Custom Attribute Functions
#'
#' @param config list Configuration object containing custom_attributes
#' @param exp_path character Path to experiment directory
#' @return list Named list of custom attribute functions
#' @export
load_custom_functions <- function(config, exp_path) {
  # Return empty list if no custom attributes defined
  if (is.null(config$custom_attributes)) {
    return(list())
  }
  
  # Check for custom.R file existence
  custom_file <- file.path(exp_path, "custom.R")
  if (!file.exists(custom_file)) {
    stop("custom.R file not found in experiment directory")
  }
  
  # Create new environment and load custom functions
  env <- new.env()
  source(custom_file, local = env)
  
  # Process each custom attribute function
  custom_funcs <- list()
  for (attr_name in names(config$custom_attributes)) {
    # Get function name and verify existence
    func_name <- config$custom_attributes[[attr_name]]$function_name
    if (!exists(func_name, envir = env)) {
      stop(sprintf("Function '%s' not found in custom.R", func_name))
    }
    
    func <- get(func_name, envir = env)
    if (!is.function(func)) {
      stop(sprintf("'%s' must be a function", func_name))
    }
    
    # Validate function parameters
    formals <- names(formals(func))
    required_params <- c("level")
    missing_params <- setdiff(required_params, formals)
    if (length(missing_params) > 0) {
      stop(sprintf("Function '%s' must have parameter: %s", 
                   func_name, paste(missing_params, collapse = ", ")))
    }
    
    # Store function and metadata
    custom_funcs[[attr_name]] <- list(
      func = func,
      label = config$custom_attributes[[attr_name]]$attribute_label,
      function_name = func_name
    )
  }
  
  custom_funcs
}
