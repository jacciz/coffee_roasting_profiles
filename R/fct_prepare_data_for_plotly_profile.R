# TODO start at charge 0 ?? it does, just need to trim
#' Title
#'
#' @param profile opened alog df file
#'
#' @return wide df of phase times
#' @noRd
prepare_phase_times <- function(profile) {
  phase_times = get_data_of_phase_times(profile) %>%
    tidyr::pivot_longer(everything(), values_to = "phase_time") %>%
    dplyr::mutate(name = factor(.data$name, levels = c("Dry", "Mid", "Dev"))) %>% # add levels
    dplyr::mutate(
      percent = as.character(round(
        .data$phase_time * 100 / sum(.data$phase_time), 1
      )),
      # Get phase percent
      phase_time_mid = format(lubridate::as_datetime(cumsum(.data$phase_time)), "%M:%S"),
      phase_time = format(lubridate::as_datetime(.data$phase_time), "%M:%S")
    )
  # dplyr::mutate_if(is.integer, lubridate::as_datetime) %>%
  # dplyr::mutate_if(is.numeric, lubridate::as_datetime)
  # Get start times of each phase so that is where the label start. Add 1 to offset label.
  phase_times[phase_times$name == "Dry", "label_point"] = "00:00"
  phase_times[phase_times$name == "Mid", "label_point"] = phase_times[phase_times$name == "Dry", "phase_time_mid"]# + 1
  phase_times[phase_times$name == "Dev", "label_point"] = phase_times[phase_times$name == "Mid", "phase_time_mid"]
  phase_times
  }


#' put list inside a df
#'
#' @param df profile
#'
#' @return
#' @noRd
times_temps_deltas_as_df <- function(df = profile) {
  data.frame(
    "time" = unlist(df$time),
    "dtemp1" = unlist(df$dtemp1),
    "dtemp2" = unlist(df$dtemp2),
    "timex" = unlist(df$timex),
    "Time1" = unlist(df$Time1),
    "Time2" = unlist(df$Time2),
    "BT" = unlist(df$BT),
    "ET" = unlist(df$ET),
    "Event" = unlist(df$Event)
  ) # %>% dplyr::slice(-c(1:36))
}
