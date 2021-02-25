## code to prepare `my_dataset` dataset goes here
# https://community.rstudio.com/t/shiny-developer-series-episode-2-follow-up-thread-colin-fay-on-golem-and-effective-shiny-development-methods/32618/6

# read.csv("")

# arab = read.csv('data/arabica_data_cleaned.csv')
coffee_flavors = read.csv("data/sunburst-coffee-flavors-complete.csv")
coffee_cupping_tasting = readxl::read_xlsx('data/coffee-flavors_lexicon.xlsx')

# usethis::use_data(arab, overwrite = TRUE)
usethis::use_data(coffee_flavors, overwrite = TRUE)
usethis::use_data(coffee_cupping_tasting, overwrite = TRUE)


haiti <- readxl::read_xlsx("data/21-02-09_2015_Haiti_Baptiste.xlsx", skip = 3) %>% select(1:6) %>% rename(change_BT = "Δ BT")
# haiti$Time2 %>% typeof()
haiti <- haiti %>% mutate(Time2 = lubridate::ms(Time2), Time1 = lubridate::ms(Time1))

# could also change height of fan
# maillard rgb(247,198,111)
haiti[grepl("Heat", haiti$Event), "event_color" ] <- "#f71212"
haiti[grepl("Fan", haiti$Event), "event_color" ] <- "#0d0dff"
haiti[grepl("FC", haiti$Event), "event_color" ] <- "#c5a872"
haiti[grepl("Dry End", haiti$Event), "event_color" ] <- "#77b36f"
haiti[grepl("Charge", haiti$Event), "event_color" ] <- "#222921"
usethis::use_data(haiti, overwrite = TRUE)
