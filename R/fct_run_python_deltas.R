# reticulate::use_virtualenv('/usr/bin/python3 ', required = TRUE)

#' Get deltas via python scrips
#' Read python functions and run it. Returns a raw df with 2 rows of delta1 and delta2
#'
#' @param alog opened alog
#' @param charge_idx integer
#' @param drop_idx integer
#'
#' @noRd
get_python_deltas <- function(alog, charge_idx, drop_idx) {
  # requireNamespace(reticulate)

  require("reticulate")
  reticulate::source_python(".//inst/app/www/get_ror_curves_r_as_input.py")
  py$get_ror_curves_r(alog, charge_idx, drop_idx)
}

#' Put python deltas into clean df
#'
#' Puts into a df with 2 delta columns. Removed NULL and NaN.
#' @param raw_deltas pandas df
#' @noRd
clean_deltas_from_python <- function(raw_deltas) {
  deltas <- raw_deltas %>% dplyr::mutate_if(is.list, as.character)
  # Make all nulls into NA
  deltas[deltas == "NULL"] <- NA
  deltas[deltas == "NaN"] <- NA
  # Columns should be same type
  deltas = deltas %>% dplyr::mutate_if(is.character, as.double)

  t1 = deltas %>% dplyr::slice(1) %>% tidyr::pivot_longer(everything(), names_to = "time", values_to = "dtemp1")
  t2 = deltas %>% dplyr::slice(2) %>% tidyr::pivot_longer(everything(), names_to = "time", values_to = "dtemp2")
  dplyr::left_join(t1, t2, by = "time")
}
# TODO add event
#' Attaches timestamp to deltas
#'
#' @param cleaned_deltas df
#' @param time_list timex from json
#' @noRd

add_times_to_delta <-
  function(cleaned_deltas,
           all_times) {
    time_list = all_times$time # 472
    time_length = length(time_list)
    cleaned_deltas %>%
      dplyr::slice_tail(n = time_length) %>%
    dplyr::mutate(timex = time_list, Time1 = all_times$Time1, Time2 = all_times$Time2, BT = all_times$BT, ET = all_times$ET, Event = all_times$Event)
  }

#' Gets final delta df
#'
#' @param time_df list of times
#' @param charge_idx integer
#' @param drop_idx integer
#'
#' @return clean long df of times and deltas
#' @noRd
extract_deltas <- function(time_df, charge_idx, drop_idx) {
  # time_with_temps <- extract_time_temps(raw_alog, charge_idx)
  deltas = get_python_deltas(time_df, charge_idx, drop_idx) %>% clean_deltas_from_python()
  add_times_to_delta(deltas, time_df)
}
