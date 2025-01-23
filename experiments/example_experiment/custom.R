#' Custom Attribute Functions for DCE Experiment
#'
#' @description
#' Functions for custom attribute display
#'
#' @author Przemyslaw Marcowski, PhD <p.marcowski@gmail.com>
#' @date 2024-01-20

#' Display Image
#'
#' @param context list Context information including choice set and column index
#' @return character HTML string for image display
#' @export
display_image <- function(context) {
  col_index <- context$col_index
  level <- as.numeric(context$choice_set["Displayed image", col_index])
  if (!is.numeric(level) || level < 1 || level > 3) return("")
  sprintf('<img src="image%d.svg" style="width:48px;height:48px;">', level)
}

#' Display Majority Choice Indicator
#'
#' @param context list Context information including choice set and column index
#' @return character HTML string for majority indicator
#' @export
display_majority_choice <- function(context) {
  choice_set <- context$choice_set
  col_index <- context$col_index

  # Get values and calculate totals
  state_vals <- as.numeric(gsub("[^0-9]", "", choice_set["What amount would the State contribute?", ]))
  employer_vals <- as.numeric(gsub("[^0-9]", "", choice_set["What amount would the employer contribute?", ]))
  totals <- state_vals + employer_vals

  # Return message only for the column with maximum total
  if (col_index == which.max(totals)) {
    return('<div style="color:#1a75ff;font-weight:bold;">Most people chose this option</div>')
  }
  return("")
}
