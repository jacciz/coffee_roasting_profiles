#' Get first line for csv
#'
#' @param wide_format profile as df
#'
#' @return string of first line
first_line_of_csv <- function(wide_format){
  sprintf("Date:%s\t Unit:%s\t CHARGE:%s\t TP:%s\t DRYe%s\t FCs%s\t FCe%s\t SCs%s\t SCe%s\t DROP%s\t COOL%s\t Time%s\t",
          wide_format$roastisodate,
          wide_format$mode,
          format(lubridate::as_datetime(wide_format$computed.TP_idx + 1), "%M:%S"),
          format(lubridate::as_datetime(wide_format$computed.TP_idx + 1), "%M:%S"),
          format(lubridate::as_datetime(wide_format$computed.DRY_time + wide_format$computed.TP_idx + 1), "%M:%S"), # 5
          format(lubridate::as_datetime(wide_format$computed.FCs_time + wide_format$computed.TP_idx + 1), "%M:%S"),
          format(lubridate::as_datetime(wide_format$computed.FCe_time + wide_format$computed.TP_idx + 1), "%M:%S"),
          ifelse(is.null(wide_format$computed.SCs_time), "", format(lubridate::as_datetime(wide_format$computed.SCs_time + wide_format$computed.TP_idx + 1), "%M:%S")),
          ifelse(is.null(wide_format$computed.SCe_time), "", format(lubridate::as_datetime(wide_format$computed.SCe_time+ wide_format$computed.TP_idx + 1), "%M:%S")),
          format(lubridate::as_datetime(wide_format$computed.DROP_time + wide_format$computed.TP_idx + 1), "%M:%S"),
          "",
          format(lubridate::as_datetime(lubridate::hms(wide_format$roasttime)), "%H:%M"))
}

# TODO
#' Formats df to save to a cSV
#'
#' @param temps_df profile as df, could get temps from df
#' @param wide_format profile as df
#'
#' @noRd
save_profile_as_csv <- function(temps_df, wide_format){
  # Must first select columns we need
  formatted_temps <- readr::format_delim(temps_df, delim = "\t")
  first_line <- first_line_of_csv(wide_format)

  together <- paste0(first_line, "\n", formatted_temps)
  writeLines(together, "data-raw/saved/test.csv")
}
