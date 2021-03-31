#' Open .alog file
#'
#' This opens the .alog file that was opened, reads it, returns string.
#' @param alog_input raw alog file
#'
#' @noRd
open_profile_as_json <-
  function(alog_input) {
    # Reads and puts it in a long string
    sub <- readLines(alog_input)
    # incomplete final line found on (c..) - could be bc you need to add line at
    # end of json Must replace ' and change True/False so it has quotes in order
    # to read as a JSON
    sub <- gsub("\\'", '"', sub)
    sub <- gsub("True", '\"True\"', sub)
    sub <- gsub("False", '\"False\"', sub)
    sub
  }

#' Get profile name
#'
#' Creates a filename based on profile, returns  with location where it is saved
#' (i.e. data/filename.json)
#' @param profile_as_json opened json file
#' @param country country input
#' @param region region input
#'
#' @noRd
get_profile_filename <- function(profile_as_json, country, region) {
  # Create filename based on roast date, country and region
  # Using date and roast time to ensure a unique filename
  sprintf(
    "%s-%s-%s-%s.json",
    # "data-raw/saved/",
    # location to save
    country,
    region,
    profile_as_json$roastisodate,
    gsub(":", "-", profile_as_json$roasttime)
  )
}

#' Opens raw alog (char)
#'
#' @param raw_alog first uploaded alog
#'
#' @return wide format df with 110 variables
#' @export
#'
#' @examples
open_profile_as_df <- function(raw_alog){
  raw_alog %>% tidyjson::spread_all()
}

#' Tidy time and temps from raw alog
#'
#' @param raw_alog first uploaded alog
#' @param charge_time charge time, must be an integer
#'
#' @return
#' @export
#'
#' @examples
extract_time_temps <- function(raw_alog, charge_time) {

  # Gather arrays we want. Columns are named as to match CSV so Artisan can read them.
  timex = raw_alog %>% tidyjson::enter_object(timex) %>% tidyjson::gather_array()
  temp1 = raw_alog %>% tidyjson::enter_object(temp1) %>% tidyjson::gather_array()
  temp2 = raw_alog %>% tidyjson::enter_object(temp2) %>% tidyjson::gather_array()
  specialevents = raw_alog %>% tidyjson::enter_object(specialevents) %>% tidyjson::gather_array()
  specialeventsStrings = raw_alog %>% tidyjson::enter_object(specialeventsStrings) %>% tidyjson::gather_array()

  # Transform times and temps, Time2 starts at Charge time. timex is for python.
  all_3 = data.frame(Time1 = timex, ET = temp1, BT = temp2) %>%
    dplyr::select(dplyr::ends_with("..JSON")) %>%
    dplyr::mutate(timex = Time1...JSON)
  colnames(all_3) <- c("Time1", "ET", "BT", "timex")
  all_3 <- all_3  %>% dplyr::mutate_if(is.list, as.numeric)
  all_3 <-
    all_3 %>% dplyr::mutate(
      Time2 = format(lubridate::as_datetime(Time1 - charge_time), "%M:%S"),
      Time2 = ifelse(as.integer(row.names(.)) < charge_time, "", Time2)
    ) %>% dplyr::mutate(Time1 = format(lubridate::as_datetime(Time1), "%M:%S"))

  # Transform special events, i.e. heat and fan times
  event = data.frame(Time1 = specialevents, Event = specialeventsStrings) %>% dplyr::select(dplyr::ends_with("..JSON"))
  colnames(event) <- c("Time1", "Event")
  event <- event %>%
    dplyr::mutate(Time1 = as.numeric(Time1), Event = as.character(Event)) %>%
    dplyr::mutate(Time1 = format(lubridate::as_datetime(Time1), "%M:%S"))

  # Combine into one df
  combined = dplyr::left_join(all_3, event, by = "Time1") %>%
    dplyr::select(Time1, Time2, ET, BT, Event, timex)
  combined[is.na(combined)] <- ""
  combined
}

#' Puts string into a clean df, incl deltas
#'
#' @param raw_alog string of open alog file
#'
#' @return wide df with times, deltas, temps as type 'list'
#' @export
#'
#' @examples
clean_raw_alog <- function(raw_alog) {
  opened_alog <- open_profile_as_df(raw_alog)

  charge = opened_alog$computed.TP_idx
  drop = opened_alog$computed.DROP_time

  time_with_temps <-
    extract_time_temps(raw_alog, charge_time = charge_idx)

  deltas <- extract_deltas(time_with_temps, charge, drop)

  purrr::map(deltas, list) %>% rbind() %>% cbind(opened_alog, .)
}

#' Title
#'
#' @param temps_df
#' @param wide_format
#'
#' @return
#' @export
#'
#' @examples
save_profile_as_csv <- function(temps_df, wide_format){
  # Must first select columns we need
  formatted_temps <- readr::format_delim(temps_df, delim = "\t")
  first_line <- first_line_of_csv(wide_format)

  together <- paste0(first_line, "\n", formatted_temps)
  writeLines(together, "data-raw/saved/test.csv")
}
