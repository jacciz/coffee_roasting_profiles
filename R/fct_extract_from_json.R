#' Display data at upload # CHECK IF NO DROP TIME check
#'
#' Grabs certain fields for display.
#'
#' @param json_file json file that is read
#'
#' @return df in wide format
#' @export
get_data_to_display_at_upload <- function(profile) {
  weight_loss_var = ifelse(is.null(profile[["computed.weight_loss"]]), 0, profile[["computed.weight_loss"]])
  tibble::tibble(
    "Roast date: " = as.character(profile[["roastisodate"]]),
    "Roast time: " = as.character(profile[["roasttime"]]),
    # "Weight before: " = as.character(profile[["computed.weightin"]]),
    # "Weight after: " = as.character(profile[["computed.weightout"]]),
    "Weight loss: " = paste0(weight_loss_var, "%"),
    # "Weight units: " = as.character(profile[["weight"]]),
    "Bean notes: " = as.character(profile[["beans"]]) # notes
  )
}

#' Get 3 phase lengths
#'
#' @inheritParams get_data_to_display_at_upload
#'
#' @export
get_data_of_phase_times <- function(profile) {
  if (length(grep("^computed.*phasetime$", names(profile))) != 3) {
    return(tibble::tibble(Dry = 0, Mid = 0, Dev = 0))
  } else{
    tibble::tibble(Dry = profile[["computed.dryphasetime"]],
                   Mid = profile[["computed.midphasetime"]],
                   Dev = profile[["computed.finishphasetime"]])
  }
}

