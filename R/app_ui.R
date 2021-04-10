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
      #  -------------- The Roast Profile Tab -------------------- ####
      tabPanel("Compare roast profiles",
               sidebarLayout(
                 column(
                   width = 3,
                   sidebarPanel(
                     width = 12,
                     tags$span(
                       HTML(
                         '<img src="www/noun_Downloadfile_1528035.png" style="width:24px;height:24px;">
                  </i><h4 style="display:inline-block;">&ensp;Load File</h4>'
                       )
                     ),
                     shiny::fileInput(
                       inputId = 'roast_curves_upload_multiple',
                       'Upload Artisan (.alog)',
                       accept = '.alog'
                     )                      # htmlOutput("get_filenames_saved")
                   ),
                   sidebarPanel(
                     width = 12,
                     tags$span(
                       HTML(
                         '<img src="www/noun_Coffee.png" style="width:24px;height:24px;">
                  </i><h4 style="display:inline-block;">&ensp;Roast Summary</h4>'
                       )
                     ),
                     # style = "overflow-x: scroll;"
                   )
                 ),
                 mainPanel(
                   fluidRow(
                    width = NULL,
                    column(width = 12,
                          style = "text-align:justify; color:black;
                          background-color:#fff;padding:15px;border-radius:10px",
                          # mod_chart_roasting_profile_ui("roast_profile_chart"))
                          mod_chart_roasting_profile_ui("uploaded_profile"),
                          mod_DT_summary_table_ui("summary_table")
                          )
                   ))
                 )),
      tabPanel("Examine roast profile",
               sidebarLayout(
                 column(
                   width = 3,
                   sidebarPanel(
                     width = 12,
                     tags$span(
                       HTML(
                         '<img src="www/noun_Downloadfile_1528035.png" style="width:24px;height:24px;">
                  </i><h4 style="display:inline-block;">&ensp;Load File</h4>'
                       )
                     ),
                     shiny::fileInput(
                       inputId = 'roast_curves_upload_single',
                       'Upload Artisan (.alog)',
                       accept = '.alog'
                     )                      # htmlOutput("get_filenames_saved")
                   ),
                   sidebarPanel(
                     width = 12,
                     tags$span(
                       HTML(
                         '<img src="www/noun_Coffee.png" style="width:24px;height:24px;">
                  </i><h4 style="display:inline-block;">&ensp;Roast Summary</h4>'
                       )
                     ),
                     mod_chart_summary_table_template_ui("uploaded_data_preview_single")
                     # mod_chart_summary_table_template_ui("roasting_db_data")
                     # style = "overflow-x: scroll;"
                   )
                 ),
                 mainPanel(
                   fluidRow(
                     width = NULL,
                     column(width = 12,
                            style = "text-align:justify; color:black;
                          background-color:#fff;padding:15px;border-radius:10px",
                            # mod_chart_roasting_profile_ui("roast_profile_chart"))
                            mod_chart_roasting_profile_ui("uploaded_profile_single"),
                            mod_DT_summary_table_ui("summary_table_single")
                     )
                   ))
               )),
               tabPanel(
                 tagList(tags$span(
                   HTML(
                     '<img src="www/noun_sunburstplot_2797256.png" style="width:24px;height:24px;"></i>'
                   )
                 ), "Tasting Charts"),
                 plotlyOutput("coffee_tasting", height = "600px"),
                 plotlyOutput("coffee_flavors", height = "600px")
               )# textOutput('hover')
             )
    )
    #  -------------- Upload Data Tab --------------------
    # https://mastering-shiny.org/action-transfer.html
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

