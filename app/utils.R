#' DCE Application Utility Functions
#'
#' @description
#' Utility functions for the DCE Shiny application
#'
#' @author Przemyslaw Marcowski, PhD <p.marcowski@gmail.com>
#' @date 2024-12-24

#' Select Choice Set
#'
#' @param V reactive values object
#' @param design matrix design matrix
#' @param bs numeric vector begin indices
#' @param es numeric vector end indices
#' @param atts data.frame attributes
#' @param atts_lvls numeric vector attribute levels
#' @param atts_coding character vector coding types
#' @param n_atts integer number of attributes
#' @param config list configuration parameters
#' @return matrix current choice set
#' @export
select_choice_set <- function(rv, design, bs, es, atts, atts_lvls, atts_coding,
                              n_atts, config) {
  set <- design[bs[rv$trial]:es[rv$trial], ]
  choice_set <- idefix::Decode(
    des = set,
    n.alts = config$design$n_alts,
    lvl.names = rv$atts_labs,
    coding = atts_coding,
    alt.cte = config$design$alt_cte
  )[[1]]

  # Format choice set with original column names and preserve original attribute names
  choice_set <- t(choice_set[, 1:n_atts])
  colnames(choice_set) <- config$design$alternatives
  rownames(choice_set) <- names(atts) # Use original names from attributes.csv

  # Apply custom attributes if configured
  if (!is.null(rv$custom_funcs)) {
    choice_set <- apply_custom_attributes(choice_set, rv$custom_funcs, config)
  }

  # Apply shuffle if configured
  if (config$ui$shuffle_attributes) {
    choice_set <- choice_set[sample(nrow(choice_set)), , drop = FALSE]
  }

  return(choice_set)
}

#' Load Custom Attribute Functions
#'
#' @param config list Configuration object containing custom_attributes
#' @param resources_path character Path to experiment resources directory
#' @return list Named list of custom attribute functions
#' @export
load_custom_functions <- function(config, resources_path) {
  # Return empty list if no custom attributes defined
  if (is.null(config$custom_attributes)) {
    return(list())
  }

  # Check for custom.R file existence
  custom_file <- file.path(resources_path, "custom.R")
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
    required_params <- c("context")
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

#' Apply custom attribute functions to choice set
#' @param choice_set matrix Current choice set matrix
#' @param custom_funcs list Custom attribute functions
#' @param config list configuration parameters
#' @return matrix Modified choice set
#' @export
apply_custom_attributes <- function(choice_set, custom_funcs, config) {
  if (length(custom_funcs) == 0) return(choice_set)

  result <- choice_set

  # Process each custom attribute
  for (attr_name in names(custom_funcs)) {
    func_info <- custom_funcs[[attr_name]]
    if (is.null(func_info$label)) next

    # Create context object
    context <- list(
      choice_set = choice_set,
      config = config,
      alternatives = config$design$alternatives
    )

    # Apply function to each column
    attr_values <- vapply(seq_len(ncol(choice_set)), function(col) {
      context$col_index <- col
      tryCatch({
        func_info$func(context = context)
      }, error = function(e) {
        warning(sprintf("Error in custom function '%s': %s", attr_name, e$message))
        ""
      })
    }, character(1))

    # Update result matrix if we got any non-empty values
    if (any(attr_values != "")) {
      if (func_info$label %in% rownames(result)) {
        result <- result[rownames(result) != func_info$label, , drop = FALSE]
      }
      new_row <- matrix(attr_values,
                       nrow = 1,
                       dimnames = list(func_info$label, colnames(choice_set)))
      result <- rbind(result, new_row)
    }
  }

  return(result)
}

#' Save Experiment Data
#'
#' @param data list experiment data
#' @param config list configuration parameters
#' @param exp_id character experiment identifier
#' @param n_atts integer number of attributes
#' @return NULL
#' @export
save_experiment_data <- function(data, config, exp_id, n_atts) {
  # Format data for all storage methods
  survey_rows <- nrow(data$survey)
  formatted_data <- data.frame(
    set = rep(1:length(data$responses), each = survey_rows),
    as.data.frame(data$survey, stringsAsFactors = FALSE, check.names = FALSE),
    response = rep(data$responses, each = survey_rows),
    row.names = NULL
  )

  # Generate filename with timestamp
  timestamp <- as.integer(Sys.time())
  filename <- sprintf("%s_%s.txt", exp_id, timestamp)

  # Process configured storage providers
  storage_providers <- names(config$storage)
  for (provider in storage_providers) {
    if (provider == "s3") {
      tryCatch({
        save_to_s3(formatted_data, filename, config$storage$s3, exp_id)
      }, error = function(e) {
        warning(sprintf("Failed to save data to S3: %s", e$message))
      })
    }
  }

  invisible(NULL)
}

#' Save Data to S3
#'
#' @param data data.frame data to save
#' @param filename character filename
#' @param s3_config list S3-specific configuration parameters
#' @param exp_id character experiment identifier
#' @return NULL
#' @export
save_to_s3 <- function(data, filename, s3_config, exp_id) {
  # Create temporary file
  temp_file <- tempfile(fileext = ".txt")
  write.table(
    data,
    temp_file,
    row.names = FALSE,
    quote = FALSE,
    sep = "\t",
    col.names = TRUE,
    fileEncoding = "UTF-8"
  )

  # Construct S3 object key with prefix
  object_key <- file.path(
    s3_config$prefix,
    exp_id,
    filename
  )

  # Set AWS credentials
  withr::with_envvar(
    new = c(
      "AWS_ACCESS_KEY_ID" = s3_config$access_key,
      "AWS_SECRET_ACCESS_KEY" = s3_config$secret_key,
      "AWS_DEFAULT_REGION" = s3_config$region
    ),
    {
      aws.s3::put_object(
        file = temp_file,
        object = object_key,
        bucket = s3_config$bucket
      )
    }
  )

  # Clean up
  unlink(temp_file)

  invisible(NULL)
}
