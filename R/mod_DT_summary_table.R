#' DT_summary_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_DT_summary_table_ui <- function(id){
  tagList(DT::dataTableOutput(shiny::NS(id, "DT_summary_table")))
}

#' DT_summary_table Server Function
#'
#' @noRd
mod_DT_summary_table_server <-
  function(id, DT_table) {
    shiny::moduleServer(id, function(input, output, session) {
      output$DT_summary_table <- DT::renderDataTable({
        DT::datatable(
          DT_table,
          rownames = FALSE,
          selection = 'single',
          colnames = c("Roast date", "Roast time", "Dry Time")
        ) %>% DT::formatStyle(
          'computed.DRY_time',
          background = DT::styleColorBar(DT_table$computed.DRY_time, 'steelblue'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
      })
    })
  }

## To be copied in the UI
# mod_DT_summary_table_ui("DT_summary_table_ui_1")

## To be copied in the server
# callModule(mod_DT_summary_table_server, "DT_summary_table_ui_1")

