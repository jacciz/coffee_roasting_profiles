#' Display data at upload # CHECK IF NO DROP TIME check
#'
#' @param json_file json files that was uploaded
#'
#' @noRd
#' @export
get_data_to_display_at_upload <- function(json_file) {
  weight_loss_var = ifelse(is.null(json_file[["computed"]][["weight_loss"]]), 0, json_file[["computed"]][["weight_loss"]])
  tibble::tibble(
    "Roast date: " = json_file[["roastisodate"]],
    "Roast time: " = json_file[["roasttime"]],
    "Weight before: " = as.character(json_file[["computed"]][["weightin"]]),
    "Weight after: " = as.character(json_file[["computed"]][["weightout"]]),
    "Weight loss: " = paste0(weight_loss_var, "%"),
    "Weight units: " = as.character(json_file[["weight"]][3]),
    "Bean notes: " = json_file[["beans"]] # notes
  )
}

#' Get special events and times
#'
#' @inheritParams get_data_to_display_at_upload
#' @export
get_special_event_times <- function(json_file) {
  tibble::tibble(time_of_event = as.character(json_file[["specialevents"]]),
         type_of_event = as.character(json_file[["specialeventsStrings"]]))
}

#' Get times and temps for graph
#'
#' @inheritParams get_data_to_display_at_uploade
#'
#' @noRd
#' @export
get_data_of_times_temps <- function(json_file) {
  tibble::tibble(time = as.character(json_file[["timex"]]),
         BT = as.character(json_file[["temp1"]]),
         ET = as.character(json_file[["temp2"]]))
}

#' Get tp, dry, fc, sc, drop times
#'
#' @inheritParams get_data_to_display_at_upload
#'
#' @noRd
#' @export
get_event_times <- function(json_file) {
  tibble::tibble(
    tp_time = json_file[["computed"]][["TP_time"]],
    dry_time = json_file[["computed"]][["DRY_time"]],
    fc_time_start = ifelse(is.null(json_file[["computed"]][["FCs_time"]]), 0, json_file[["computed"]][["FCs_time"]]),
    fc_time_end = ifelse(is.null(json_file[["computed"]][["FCe_time"]]), 0, json_file[["computed"]][["FCe_time"]]),
    sc_time_start = ifelse(is.null(json_file[["computed"]][["SCs_time"]]), 0, json_file[["computed"]][["SCs_time"]]),
    sc_time_end = ifelse(is.null(json_file[["computed"]][["SCe_time"]]), 0, json_file[["computed"]][["SCe_time"]]),
    drop_time = json_file[["computed"]][["DROP_time"]]
  )
}

#' Get 3 phase lengths
#'
#' @inheritParams get_data_to_display_at_upload
#'
#' @noRd
#' @export
get_data_of_phase_times <- function(json_file) {
  tibble::tibble(
    dryphase = json_file[["computed"]][["dryphasetime"]],
    midphase = json_file[["computed"]][["midphasetime"]],
    developphase = json_file[["computed"]][["finishphasetime"]]
  )
}

