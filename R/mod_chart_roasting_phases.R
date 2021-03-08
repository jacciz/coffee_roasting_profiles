#' chart_roasting_phases UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_chart_roasting_phases_ui <- function(id) {
  ns <- NS(id)
  tagList(plotly::plotlyOutput(ns("roast_phases"), height = "50px"))
}

# Should line this up above 500 degrees

#' chart_roasting_profile Server Function
#'
#' @noRd
#'
#'
mod_chart_roasting_phases_server <- function(id, json_file) {
  moduleServer(id, function(input, output, session) {
    output$roast_phases <-
      plotly::renderPlotly({
        times = get_data_of_phase_times(json_file()) %>% tidyr::pivot_longer(everything(), values_to = "phase_time") %>% dplyr::mutate(group = "a", name = factor(name, levels = c("dryphase","midphase","developphase")))
        times$name

        colors <- c('#4DB848', '#FF9673', '#E1C8B4')

        plotly::plot_ly(
          times,
          # x =~name,
          y = ~ group,
          name =  ~ name,
          orientation = 'h',
          hovertemplate = '%{x: .1f} s',
          type = 'bar'
        ) %>%
          plotly::add_trace(x =~phase_time, marker = list(color = colors)) %>%
          plotly::layout(barmode = 'stack') %>% plotly::layout(
            showlegend = FALSE,
            xaxis = list(
              title = "",
              showticklabels = FALSE,
              zeroline = F,
              showline = F,
              showgrid = F),
            yaxis = list(
              title = "",
              showticklabels = FALSE,
              zeroline = F,
              showline = F,
              showgrid = F
            ),
            margin = list(
              r = 0,
              l = 55,
              b = 0,
              t = 0
            ),
            plot_bgcolor = 'rgb(245,245,245)',
            # make grey background
            paper_bgcolor = 'rgb(245,245,245)'
          )

    })
    # }
  })
}


