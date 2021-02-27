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
