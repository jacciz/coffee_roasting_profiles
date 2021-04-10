get_DT_table <- function(opened_profile){
  opened_profile %>% dplyr::select(roastisodate, roasttime, computed.DRY_time)
}
