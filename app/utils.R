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

#' Apply custom attribute functions to choice set
#' @param choice_set matrix Current choice set matrix
#' @param custom_funcs list Custom attribute functions
#' @param config list configuration parameters
#' @return matrix Modified choice set
#' @export
apply_custom_attributes <- function(choice_set, custom_funcs, config) {
  if (length(custom_funcs) == 0) return(choice_set)

  result <- choice_set
  processed_rows <- character(0)

  # Process each custom attribute
  for (attr_name in names(custom_funcs)) {
    func_info <- custom_funcs[[attr_name]]

    # Skip if no label specified
    if (is.null(func_info$label)) next

    # Process each column
    attr_values <- vapply(1:ncol(choice_set), function(col_idx) {
      # Get the level value from the original attribute if it exists
      level <- if (func_info$label %in% rownames(choice_set)) {
        as.numeric(choice_set[func_info$label, col_idx])
      } else {
        NULL
      }

      # Create the context list with only the necessary elements
      context <- list(
        choice_set = choice_set,
        col_index = col_idx,
        config = config,
        alternatives = config$design$alternatives
      )

      tryCatch({
        # Call the function with level and context as named arguments
        do.call(func_info$func, list(level = level, context = context))
      }, error = function(e) {
        warning(sprintf("Error in custom function '%s': %s", attr_name, e$message))
        ""
      })
    }, character(1))

    # Remove existing row if it exists and add new values
    if (func_info$label %in% rownames(result)) {
      result <- result[!rownames(result) == func_info$label, , drop = FALSE]
    }

    # Add non-empty results as new row
    if (any(!attr_values == "")) {
      new_row <- matrix(attr_values,
                       nrow = 1,
                       dimnames = list(func_info$label, colnames(choice_set)))
      result <- rbind(result, new_row)
      processed_rows <- c(processed_rows, func_info$label)
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
