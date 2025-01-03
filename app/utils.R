#' DCE Application Helper Functions
#'
#' @description
#' Helper functions for the DCE Shiny application:
#' - Choice set selection and formatting
#' - Data saving and manipulation
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
  # Get current set
  set <- design[bs[V$sn]:es[V$sn], ]
  
  # Decode design
  choice_set <- idefix::Decode(
    des = set,
    n.alts = config$design$n_alts,
    lvl.names = V$atts_labs,
    coding = atts_coding,
    alt.cte = config$design$alt_cte
  )[[1]]
  
  # Format choice set
  choice_set <- t(choice_set[, 1:n_atts])
  colnames(choice_set) <- config$design$alternatives
  rownames(choice_set) <- names(atts_lvls)
  
  return(choice_set)
}

#' Convert Responses to Binary Format
#'
#' @param resp character vector responses
#' @param alts character vector alternatives
#' @param n_alts integer number of alternatives
#' @return numeric vector binary responses
#' @export
convert_responses <- function(resp, alts, n_alts) {
  if (!all(resp %in% alts)) {
    stop("One or more responses do not match the possible options")
  }
  
  map <- match(resp, alts)
  responses <- vector("list", length = length(map))
  
  for (i in seq_along(map)) {
    responses[[i]] <- rep(0, n_alts)
    responses[[i]][map[i]] <- 1
  }
  
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
  # Transform data to numeric format
  d <- as.data.frame(cbind(
    data$design,
    resp = data$bin_responses
  ))
  
  # Transform data to character format
  unc_resp <- rep(data$responses, each = n_atts)
  unc_setnr <- rep(1:length(data$responses), each = n_atts)
  unc_d <- cbind(
    set = unc_setnr,
    data$survey,
    resp = unc_resp
  )
  
  # Create filenames
  timestamp <- as.integer(Sys.time())
  num_name <- sprintf("%s_num_data.txt", timestamp)
  char_name <- sprintf("%s_char_data.txt", timestamp)
  
  # Save files
  save_to_dropbox(d, num_name, config, exp_id, token)
  save_to_dropbox(unc_d, char_name, config, exp_id, token)
  
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
  # Create temporary file
  path <- file.path(tempdir(), filename)
  write.table(data, path, row.names = TRUE, quote = FALSE,
              sep = "\t", col.names = NA)
  
  # Upload to Dropbox
  dropbox_path <- file.path(
    config$storage$dropbox$base_path,
    config$storage$dropbox$data_path,
    exp_id
  )
  
  rdrop2::drop_upload(path, path = dropbox_path, dtoken = token)
  
  invisible(NULL)
}
