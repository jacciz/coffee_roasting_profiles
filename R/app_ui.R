#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny formattable plotly shinyWidgets shinythemes
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    navbarPage(
      theme = shinytheme("united"),
      "Coffee",
      #  -------------- The Roast Profile Tab --------------------
      tabPanel(
        "Roast profiles",
        sidebarLayout(
          column(width = 2,
                 sidebarPanel(width = NULL,
                              radioButtons(
                                "plotType", "Plot type",
                                c("Scatter" = "p", "Line" = "l")
                              )),
                 sidebarPanel(width = NULL,
                              formattableOutput("roasting_profile_data")

                              # "Dry End: ", textOutput("dry_end", inline = TRUE),br(),
                              # "First Crack: ", textOutput("dry_end2", inline = TRUE)
                 )),
          mainPanel(width = 8, style="text-align:justify;color:black;background-color:rgb(245,245,245);padding:15px;border-radius:10px",
                    # plotlyOutput("roast_profile", height = "500px")
                    mod_chart_roasting_profile_ui("roast_profile_chart")
                    # DT::dataTableOutput("scatter2")
          )
        )
      ),
      #  -------------- Tasting Tab --------------------
      tabPanel("Tasting",
               fluidRow(
                 column(
                   width = 6,
                   plotlyOutput("coffee_tasting", height = "1000px"),
                   # textOutput('hover')
                   textOutput('click')
                   # textInput("names")
                 ),
                 column(width = 6,
                        plotlyOutput("coffee_flavors", height = "1000px"))
               )),

      #  -------------- Upload Data Tab --------------------
      tabPanel(
        "Import Data",
        shinydashboard::box(
          titlePanel("Input Profile"),
            width = 8,
            style = "text-align:justify;color:black;background-color:rgb(245,245,245);padding:15px;border-radius:10px",
            fluidRow(
              column(3, shiny::dateInput("roast_date", "Roast date", value = Sys.Date(), format = "M d, yyyy", autoclose=TRUE)),
              # input sys date
              column(3, textInput("name", "Your name")),
              column(2, numericInput("weight_before", "Weight before", value = NA)),
              column(2, numericInput("weight_after", "Weight after", value = NA)),
              column(2, pickerInput("unit_of_measure", "Units", choices = units_of_measures, options = pickerOptions())),
            ),
            fluidRow(
              column(3, pickerInput("roast_machine", "Roast machine", choices = coffee_roasting_machines)),
              column(3, textInput("roast_farm", "Farm")),
              column(3, pickerInput("country", "Country", choices = coffee_producting_countries)),
              column(3, textInput("region", "Region"))
            ),
            fluidRow(
              column(3, textInput("quality", "Quality")),
              column(3, pickerInput("processing_method", "Processing methods", choices = processing_methods)),
              column(3, selectInput("variety", "Variety", choices = coffee_varieties_arabica, multiple = TRUE)),
              column(3, textAreaInput("roast_notes", "Notes")),

            ), # https://mastering-shiny.org/action-transfer.html
            fluidRow(shiny::fileInput(inputId = 'roast_curves_upload', 'Upload Artisan (.alog)', accept = '.alog'),
                     actionButton("update_record", "Submit", class = "btn-primary")
            ),
            formattableOutput("uploaded_data_preview")
        )
      )
      #  -------------- More Tabs -dropdown --------------------
      # navbarMenu(
      #     "More",
      #     tabPanel("Table",
      #              DT::dataTableOutput("table")),
      #     tabPanel("About",
      #              fluidRow(column(
      #                  6,
      #                  # includeMarkdown("about.md")),
      #                  column(6
      #                  )
      #              )))
      # )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @import shinyFeedback
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'coffeeroastingprofiles'
    ),
    useShinyFeedback()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

