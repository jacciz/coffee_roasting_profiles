# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "shinydashboardPlus")

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "chart_roasting_profile" ) # Name of the module
golem::add_module( name = "name_of_module2" ) # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "get_python_deltas" )
golem::add_utils( "json_helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE )

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("coffeeroastingprofiles")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

# Resources:
# Remote file servers: https://rpubs.com/berdaniera/shinyshop-remotedata


# list(
#   name = reactive(input$var),
#   value = reactive(data()[[input$var]])
# )

# inheritParams
# library(shinyvalidate)
# library(shinyFeedback) # https://github.com/merlinoa/shinyFeedback and https://mastering-shiny.org/action-feedback.html, good for a single input, validate() is for output

# filter(.data[[input$var]] > .env$input$min)) or diamonds %>% filter(.data[[var]] > .env$min)

# create dynamic and multiple dropwdowns: https://mastering-shiny.org/action-dynamic.html
# ctrl shift enter
# jobs in C:\Users\dotjaz\Documents\RStudio\background-jobs view: rstudioapi::viewer("http://127.0.0.1:5101")
# traceback() to find error location, add browser() in code to launch debugger
# breakpoints - debugger tool - press n,c,q
# use message() glue::glue() str()
# selectVarServer("var", reactive(input$x)))
# ctrl .  to find function
# Testing: https://mastering-shiny.org/scaling-testing.html
# more testing https://speakerdeck.com/colinfay/workshop-building-successful-shiny-apps-with-golem?slide=130
# Performance: https://mastering-shiny.org/performance.html

