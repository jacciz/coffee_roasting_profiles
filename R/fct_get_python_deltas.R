# reticulate::use_virtualenv('/path/to/myhybrid-project', required = TRUE)

# Read python functions and run it. Returns a raw df with 2 rows of delta1 and delta2
get_python_deltas <- function(filename_to_open) {
  require(reticulate)
  # filename_to_open = "Haiti--2021-02-09-20-15-17.json"

  # file_loc = paste0(".//data-raw/saved/", filename_to_open)
  # C:/Users/jacci/Documents/DS 710/coffee_roasting_profilesd
  # print(file_loc)
  reticulate::source_python(".//data-raw/get_ror_curves.py")
  py$get_ror_curves(file_raw = filename_to_open)
}

# Puts into a df with 2 delta columns
clean_deltas_from_python <- function(raw_deltas) {
  deltas = raw_deltas %>% dplyr::mutate_if(is.list, as.character)
  # Make all nulls into NA
  deltas[deltas == "NULL"] <- NA
  deltas[deltas == "NaN"] <- NA
  # Columns should be same type
  deltas = deltas %>% dplyr::mutate_if(is.character, as.double)

  t1 = deltas %>% dplyr::slice(1) %>% tidyr::pivot_longer(everything(), names_to = "time", values_to = "dtemp1")
  t2 = deltas %>% dplyr::slice(2) %>% tidyr::pivot_longer(everything(), names_to = "time", values_to = "dtemp2")
  dplyr::left_join(t1, t2, by = "time")
}

