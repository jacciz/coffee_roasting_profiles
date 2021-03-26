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

        "Examine roast profile",
        sidebarLayout(
          column(
            width = 3,
            sidebarPanel(width = 12,
                         tags$span(
                           HTML(
                             '<img src="www/noun_Downloadfile_1528035.png" style="width:24px;height:24px;">
                  </i><h4 style="display:inline-block;">&ensp;Load File</h4>'
                           )),
                         htmlOutput("get_filenames_saved")),
            # sidebarPanel(
            #   width = 12,
            #   h4("Roast Statistics"),
            #   formattableOutput("roasting_profile_data")
            # ),
            sidebarPanel(
              width = 12,
              # h4("Roast Summary"),

              tags$span(
                HTML(
                  '<img src="www/noun_Coffee.png" style="width:24px;height:24px;">
                  </i><h4 style="display:inline-block;">&ensp;Roast Summary</h4>'
                )),

              # formattableOutput("roasting_db_data"),
              # gt::gt_output("roasting_db_data")
              mod_chart_summary_table_template_ui("roasting_db_data")
              # style = "overflow-x: scroll;"
            )

            # "Dry End: ", textOutput("dry_end", inline = TRUE),br(),
            # "First Crack: ", textOutput("dry_end2", inline = TRUE)
          ),
          mainPanel(
            # fluidRow(
            #   class = "panel-body",
            #
            #   width = NULL,
            #   column(
            #     width = 6,
            #     shinydashboard::box(
            #       style = "text-align:justify;color:black;background-color:#fff;padding-bottom:15px;border-radius:10px",
            #       width = 12,
            #       # h4("Tasting Notes"),
            #       tags$span(
            #         HTML(
            #           '<img src="www/noun_sunburstplot_2797256.png" style="width:24px;height:24px;">
            #       </i><h4 style="display:inline-block;">&ensp;Tasting Notes</h4>'
            #         )),
            #
            #       # tags$div(
            #       #   class = "panel panel-default",
            #       #   tags$div(
            #       #     class = "panel-heading",
            #       #     icon("trash"),
            #       #     "Remove item"
            #       #   ),
            #       #   tags$div(
            #       #     class = "panel-body",
            #       #     id = "remove_tasting"
            #       #   )
            #       # ),
            #
            #       # textOutput('click'),
            #
            #      # uiOutput("bucketlist_tasting")
            #
            #       # sortable::bucket_list(
            #       #   header = "Aromas",
            #       #   group_name = "bucket_list_tasting",
            #       #   orientation = "horizontal",
            #       #   sortable::add_rank_list(
            #       #     text = "",
            #       #     labels = list(textOutput('click')),
            #       #     # labels = list("one",
            #       #     #               "two",
            #       #     #               "three"),
            #       #     input_id = "rank_list_tasting",
            #       #     options = sortable::sortable_options(
            #       #       onLoad = htmlwidgets::JS("function (evt) {this.el.addChild(evt.item);}"),
            #       #       onUpdate = htmlwidgets::JS("function (evt) {this.el.addChild(evt.item);}")
            #       #     )
            #       #   ),
            #       #   sortable::add_rank_list(
            #       #     text = "Drag to Remove",
            #       #     labels = NULL,
            #       #     input_id = "remove_tasting",
            #       #     options = sortable::sortable_options(
            #       #       onAdd = htmlwidgets::JS("function (evt) {this.el.removeChild(evt.item);}")
            #       #     )
            #       #   )
            #       # )
            #     )
            #   ),
            #   column(
            #     width = 6,
            #     tags$div(
            #       class = "panel panel-default",
            #       tags$div(
            #         class = "panel-heading",
            #         icon("arrow-right"),
            #         "Drag from here (items will clone)"
            #       ))
            #     # shinydashboard::box(style = "text-align:justify;color:black;background-color:#fff;padding-bottom:15px;border-radius:10px",
            #     #                     width = 12,    tags$span(
            #     #                       HTML(
            #     #                         '<img src="www/noun_hotcup_1885847.png" style="width:24px;height:24px;">
            #     #   </i><h4 style="display:inline-block;">&ensp;Aroma Notes</h4>'
            #     #                       )))
            #   )
            # ),
            # br(),
            fluidRow(
              width = NULL,
              column(
                width = 12,
                style = "text-align:justify;color:black;background-color:#fff;padding:15px;border-radius:10px",
                tabsetPanel(
                  tabPanel(
                    "Profile",
                    mod_chart_roasting_profile_ui("roast_profile_chart")
                  ),
                  tabPanel(
                    tagList(tags$span(
                      HTML(
                        '<img src="www/noun_sunburstplot_2797256.png" style="width:24px;height:24px;"></i>'
                      )
                    ), "Tasting"),
                    # "Tasting",
                    # icon =
                    uiOutput("bucketlist_tasting"),
                    plotlyOutput("coffee_tasting", height = "600px"),
                    # textOutput('hover')

                  ),
                  tabPanel("Aromas",
                           plotlyOutput("coffee_flavors", height = "600px"))
                )
              )
            ))
          )
      ),
      tabPanel("Compare profiles"),
      #  -------------- Tasting Tab --------------------
      # Dropdowns
      # htmlOutput("get_data_upload_date"),
      # htmlOutput("get_country"),
      # htmlOutput("get_processing_method")
      # tabPanel("Tasting",
      #          fluidRow(
      #            column(
      #              width = 6,
      #              plotlyOutput("coffee_tasting", height = "1000px"),
      #              # textOutput('hover')
      #              textOutput('click')
      #              # textInput("names")
      #            ),
      #            column(width = 6,
      #                   plotlyOutput("coffee_flavors", height = "1000px"))
      #          )),

      #  -------------- Upload Data Tab --------------------
      tabPanel(
        "Import Data",
        shinydashboard::box(
          titlePanel("Input Profile"),
            width = 6,
            style = "text-align:justify;color:black;background-color:#fff;padding:15px;border-radius:10px",
            fluidRow(
              # column(3, shiny::dateInput("roast_date", "Roast date", value = Sys.Date(), format = "M d, yyyy", autoclose=TRUE)),
              # input sys date
              column(3, textInput("name", "Your name")),
              # column(2, numericInput("weight_before", "Weight before", value = NA)),
              # column(2, numericInput("weight_after", "Weight after", value = NA)),
              # column(2, pickerInput("unit_of_measure", "Units", choices = units_of_measures, options = pickerOptions()))
            ),
            fluidRow(
              column(3, pickerInput("roast_machine", "Roast machine", choices = coffee_roasting_machines)),
              column(3, textInput("roast_farm", "Farm")),
              column(3, pickerInput("country", "Country", choices = coffee_producting_countries)),
              column(3, textInput("region", "Region"))
            ),
            fluidRow(
              column(3, textInput("quality", "Quality/Grade")),
              column(3, pickerInput("processing_method", "Processing methods", choices = processing_methods)),
              column(3, selectInput("variety", "Variety", choices = coffee_varieties_arabica, multiple = TRUE)),
              # column(3, textAreaInput("roast_notes", "Notes")),

            ), # https://mastering-shiny.org/action-transfer.html
            fluidRow(shiny::fileInput(inputId = 'roast_curves_upload', 'Upload Artisan (.alog)', accept = '.alog'),
                     actionButton("save_record", "Submit profile data", class = "btn-primary")
            )),
        shinydashboard::box(
            # formattableOutput("uploaded_data_preview")
          titlePanel("Summary of uploaded profile"),
          width = 6, style = "text-align:justify;color:black;background-color:#fff;padding:15px;border-radius:10px",
        # tableOutput("uploaded_data_preview")
        mod_chart_summary_table_template_ui("uploaded_data_preview")
        ))

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

