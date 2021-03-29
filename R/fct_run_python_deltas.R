# reticulate::use_virtualenv('/usr/bin/python3 ', required = TRUE)

#' Get deltas via python scrips
#' Read python functions and run it. Returns a raw df with 2 rows of delta1 and delta2
#' @param filename_to_open complete file location and filename 'folder/file.json'
#' @noRd
get_python_deltas <- function(filename_to_open) {
  # requireNamespace(reticulate)
  require("reticulate")
  reticulate::source_python(".//inst/app/www/get_ror_curves.py")
  py$get_ror_curves(file_raw = filename_to_open)
}

#
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
    dplyr::mutate(timex = time_list, BT = all_times$BT, ET = all_times$ET)
  }
