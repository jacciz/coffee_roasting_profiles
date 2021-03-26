#' chart_summary_table_template UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_chart_summary_table_template_ui <- function(id){
  tagList(gt::gt_output(shiny::NS(id, "gt_template")))
}



#' chart_summary_table_template Server Function
#'
#' @noRd
mod_chart_summary_table_template_server <-
  function(id, data_long_format) {
    shiny::moduleServer(id, function(input, output, session) {
        output$gt_template <- gt::render_gt({
          if (is.null(data_long_format)) {
            gt::gt_preview(gt::pizzaplace)} else {
          data_long_format %>%
            gt::gt() %>%
            gt::tab_style(
              style = list(
                gt::cell_borders(
                  sides = "all",
                  color = "white",
                  style = "solid",
                  weight = gt::px(0)
                ),
                gt::cell_text(font = "Verdana", size = "14px") # "0.8vw"
              ),
              locations = gt::cells_body(columns = everything(),
                                         rows = everything())
            ) %>%
            gt::tab_style(gt::cell_text(weight = "bold"),
                          locations = gt::cells_body(columns = 1)) %>%
            gt::tab_options(
              column_labels.hidden = TRUE,
              table_body.border.bottom.color = "white",
              table_body.border.top.color = "white"
            )
            }
        })

    })
}
