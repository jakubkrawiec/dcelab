#' Custom Attribute Functions for DCE Experiment
#'
#' @description
#' Functions for custom attribute display
#'
#' @author Przemyslaw Marcowski, PhD <p.marcowski@gmail.com>
#' @date 2024-01-20

#' Display Image
#'
#' @param level numeric Level value (1-3)
#' @param context list Context information including choice set and column index
#' @return character HTML string for image display
#' @export
display_image <- function(level, context) {
  if (!is.numeric(level) || level < 1 || level > 3) return("")
  sprintf('<img src="image%d.svg" style="width:48px;height:48px;">', level)
}

#' Display Majority Choice Indicator
#'
#' @param level Ignored
#' @param context list Context information including choice set and column index
#' @return character HTML string for majority indicator
#' @export
display_majority_choice <- function(level, context) {
  if (is.null(context$choice_set) || is.null(context$col_index)) return("")

  choice_set <- context$choice_set
  col_index <- context$col_index

  # Get values and calculate totals
  state_vals <- as.numeric(gsub("[^0-9]", "", choice_set["What amount would the State contribute?", ]))
  employer_vals <- as.numeric(gsub("[^0-9]", "", choice_set["What amount would the employer contribute?", ]))
  totals <- state_vals + employer_vals

  # Select the first column that has the maximum total
  selected_index <- which.max(totals)

  # Return message only for the selected option
  if (col_index == selected_index) {
    return('<div style="color:#1a75ff;font-weight:bold;">Most people chose this option</div>')
  }

  return("")
}
