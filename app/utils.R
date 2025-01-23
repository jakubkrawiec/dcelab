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
select_choice_set <- function(V, design, bs, es, atts, atts_lvls, atts_coding,
                              n_atts, config) {
  set <- design[bs[V$sn]:es[V$sn], ]
  choice_set <- idefix::Decode(
    des = set,
    n.alts = config$design$n_alts,
    lvl.names = V$atts_labs,
    coding = atts_coding,
    alt.cte = config$design$alt_cte
  )[[1]]

  # Format choice set with original column names and preserve original attribute names
  choice_set <- t(choice_set[, 1:n_atts])
  colnames(choice_set) <- config$design$alternatives
  rownames(choice_set) <- names(atts) # Use original names from attributes.csv

  # Apply custom attributes if configured
  if (!is.null(V$custom_funcs)) {
    choice_set <- apply_custom_attributes(choice_set, V$custom_funcs, config)
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

    # Createe context object
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

#' Convert Responses to Binary Format
#'
#' @param resp character vector responses
#' @param alts character vector alternatives
#' @param n_alts integer number of alternatives
#' @return numeric vector binary responses
#' @export
convert_responses <- function(resp, alts, n_alts) {
  # Validate that all responses are valid alternatives
  if (!all(resp %in% alts)) {
    stop("One or more responses do not match the possible options")
  }

  # Convert responses to numeric indices based on alternatives
  map <- match(resp, alts)

  # Initialize list to store binary response vectors
  responses <- vector("list", length = length(map))

  # Convert each response to binary vector (1 for selected, 0 for others)
  for (i in seq_along(map)) {
    responses[[i]] <- rep(0, n_alts)
    responses[[i]][map[i]] <- 1
  }

  # Flatten list into single vector
  unlist(responses)
}

#' Save Experiment Data
#'
#' @param data list experiment data
#' @param config list configuration parameters
#' @param exp_id character experiment identifier
#' @param n_atts integer number of attributes
#' @param token rdrop2 authentication token
#' @return NULL
#' @export
save_experiment_data <- function(data, config, exp_id, n_atts, token) {
  # Save choice data with text responses
  survey_rows <- nrow(data$survey)
  choice_data <- data.frame(
    set = rep(1:length(data$responses), each = survey_rows),
    as.data.frame(data$survey, stringsAsFactors = FALSE, check.names = FALSE),
    resp = rep(data$responses, each = survey_rows),
    row.names = NULL
  )

  # Generate filename with timestamp
  timestamp <- as.integer(Sys.time())
  choice_filename <- sprintf("%s.txt", timestamp)

  # Save data file to Dropbox
  save_to_dropbox(choice_data, choice_filename, config, exp_id, token)

  invisible(NULL)
}

#' Save Data to Dropbox
#'
#' @param data data.frame data to save
#' @param filename character filename
#' @param config list configuration parameters
#' @param exp_id character experiment identifier
#' @param token rdrop2 authentication token
#' @return NULL
#' @export
save_to_dropbox <- function(data, filename, config, exp_id, token) {
  rownames(data) <- NULL

  # Create temporary file
  path <- file.path(tempdir(), filename)
  write.table(
    data, path, row.names = FALSE, quote = FALSE,
    sep = "\t", col.names = TRUE
    )

  # Upload to Dropbox
  dropbox_path <- file.path(
    config$storage$dropbox$base_path,
    config$storage$dropbox$data_path,
    exp_id
  )

  rdrop2::drop_upload(path, path = dropbox_path, dtoken = token)

  invisible(NULL)
}
