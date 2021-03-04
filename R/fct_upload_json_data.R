#' Display data at upload # CHECK IF NO DROP TIME check
#'
#' @param json_file json files that was uploaded
#'
#' @noRd
#' @export
get_data_to_display_at_upload <- function(json_file) {
  weight_loss_var = ifelse(is.null(json_file[["computed"]][["weight_loss"]]), 0, json_file[["computed"]][["weight_loss"]])
  tibble::tibble(
    "Roast date: " = as.character(json_file[["roastisodate"]]),
    "Roast time: " = as.character(json_file[["roasttime"]]),
    "Weight before: " = as.character(json_file[["computed"]][["weightin"]]),
    "Weight after: " = as.character(json_file[["computed"]][["weightout"]]),
    "Weight loss: " = paste0(weight_loss_var, "%"),
    "Weight units: " = as.character(json_file[["weight"]][3]),
    "Bean notes: " = as.character(json_file[["beans"]]) # notes
  )
}

#' Get special events and times. Subract TP_idx from all times.
#'
#' @inheritParams get_data_to_display_at_upload
#' @export
get_special_event_times <- function(json_file) {
  time_index = as.numeric(json_file[["computed"]][["TP_idx"]][[1]]) # When charge time starts
  # drop_ index = json_file[["computed"]][["DROP_time"]][[1]]
  tib <-
    tibble::tibble(time_of_event = as.numeric(json_file[["specialevents"]]),
                   type_of_event = as.character(json_file[["specialeventsStrings"]]))
  tib %>%
    dplyr::mutate(time_of_event = time_of_event - time_index) %>%
    dplyr::filter(time_of_event > 0) %>%
    dplyr::mutate(color = ifelse(grepl("^Fan", type_of_event), "#0f1fff", "#ff0f0f"))
}

#' Get times and temps for graph
#'
#' @inheritParams get_data_to_display_at_uploade
#'
#' @noRd
#' @export
get_data_of_times_temps <- function(json_file) {
  time_index = as.numeric(json_file[["computed"]][["TP_idx"]][[1]]) # When charge time starts
  tib <- tibble::tibble(time = as.numeric(json_file[["timex"]]),
         ET = as.character(json_file[["temp1"]]),
         BT = as.character(json_file[["temp2"]]))
  tib %>% dplyr::mutate(time = time - time_index) %>% dplyr::filter(time > 0)
}

#' Get tp, dry, fc, sc, drop times
#'
#' @inheritParams get_data_to_display_at_upload
#'
#' @noRd
#' @export
get_event_times <- function(json_file) {
  tibble::tibble(
    tp_time = json_file[["computed"]][["TP_time"]][[1]],
    dry_time = json_file[["computed"]][["DRY_time"]][[1]],
    fc_time_start = ifelse(is.null(json_file[["computed"]][["FCs_time"]][[1]]), 0, json_file[["computed"]][["FCs_time"]][[1]]),
    fc_time_end = ifelse(is.null(json_file[["computed"]][["FCe_time"]][[1]]), 0, json_file[["computed"]][["FCe_time"]][[1]]),
    sc_time_start = ifelse(is.null(json_file[["computed"]][["SCs_time"]][[1]]), 0, json_file[["computed"]][["SCs_time"]][[1]]),
    sc_time_end = ifelse(is.null(json_file[["computed"]][["SCe_time"]][[1]]), 0, json_file[["computed"]][["SCe_time"]][[1]]),
    drop_time = json_file[["computed"]][["DROP_time"]][[1]]
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
    dryphase = as.integer(json_file[["computed"]][["dryphasetime"]]),
    midphase = as.integer(json_file[["computed"]][["midphasetime"]]),
    developphase = as.integer(json_file[["computed"]][["finishphasetime"]])
  )
}

