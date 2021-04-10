#' chart_roasting_profile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_chart_roasting_profile_ui <- function(id) {
  ns <- NS(id)
  tagList(plotly::plotlyOutput(ns("roast_profile")))
}

#' chart_roasting_profile Server Function
#'
#' @noRd
#'
#'
mod_chart_roasting_profile_server <- function(id, profile) {
  moduleServer(id, function(input, output, session) {

  output$roast_profile <-
    plotly::renderPlotly({
      phase_times <- prepare_phase_times(profile)

      times_temps <- times_temps_deltas_as_df(profile)

      plotly::plot_ly(
        data = times_temps,
        x = ~ Time2 # Time2 starts at charge time
      ) %>%
        plotly::add_lines(
          data = times_temps,  # ET Line
          type = 'scatter',
          mode = 'lines',
          line = list(color = "#4DB848"),
          y = ~ BT,
          # hovertemplate = paste('%{y: .1f}\u00b0F', '<br>%{x}<br>'),
          hovertemplate = '%{y: .1f}\u00b0F',
          showlegend = FALSE,
          name = "BT"
        ) %>%
        plotly::add_lines(
          data = times_temps,  # ET Line
          type = 'scatter',
          mode = 'lines',
          line = list(color = "#D50032"),
          y = ~ ET,
          # hovertemplate = paste('%{y: .1f}\u00b0F', '<br>%{x}<br>'),
          hovertemplate = '%{y: .1f}\u00b0F',
          showlegend = FALSE,
          name = "ET"
        ) %>%
        plotly::layout(hovermode = "x unified") %>%
        # plotly::add_annotations( # Special events
        #   data = special_times,
        #   x =  ~ lubridate::as_datetime(as.numeric(time_of_event)),
        #   # y = ~ jitter(400, 60),
        #   y = ~ 10,
        #   text = ~ type_of_event,
        #   # yaxis = "y2",
        #   textposition = "top center",
        #   showarrow = FALSE,
        #   # arrowhead = .5,
        #   # arrowwidth = 1,
      #   font = list(size = 12, color = "#ffffff"),
      #   bgcolor = ~ color
      # ) %>%                     # Add lines for phases
      plotly::add_lines(
        data = times_temps,        # Change BT Line
        mode = 'lines',
        x = ~ Time2,
        y = ~ dtemp1,
        line = list(color = "#428BCA"),
        name = "\u0394ET",
        hovertemplate = '%{y: .1f}\u00b0F',
        showlegend = FALSE,
        yaxis = "y2"
      ) %>%
        plotly::add_lines(
          data = times_temps,        # Change ET Line
          mode = 'lines',
          x = ~ Time2,
          y = ~ dtemp2,
          line = list(color = "#3f0585"),
          name = "\u0394BT",
          hovertemplate = '%{y: .1f}\u00b0F',
          showlegend = FALSE,
          yaxis = "y2"
        ) %>%
        plotly::layout(
          xaxis = list(# gridcolor = toRGB("gray85"),
            title = "",
            zeroline = F,
            showline = F,
            showgrid = F,
            # tick0 = "00:00",
            tick0 = 0,
            ticks = "inside",
            tickcolor = "rgb(245,245,245)",
            tickformat = "%M:%S",
            # tickmode = "linear",
            dtick = 30 # Tick every 30 seconds
          ),
          # The right side y-axis
          yaxis2 = list(
            zeroline = F,
            showline = F,
            showgrid = F,
            tickfont = list(color = "#428BCA"),
            ticksuffix = "\u00b0F",
            overlaying = "y",
            side = "right",
            title = ""
          ),
          yaxis = list(
            title = "",
            ticksuffix = "\u00b0F",
            zeroline = F,
            showline = F,
            showgrid = F
          ),
          margin = list(
            r = 30,
            l = 0,
            b = 0,
            t = 0
          ),
          plot_bgcolor = '#fff',
          # make white background
          paper_bgcolor = '#fff'
        ) %>% # Add 3 phases on top
        plotly::add_bars(
          data = phase_times,
          x =~ phase_time,
          y = ~ 500,
          name =  ~ name,
          width = 25,
          text =~ name, #?
          orientation = 'h',
          showlegend = FALSE,
          marker = list(color = phase_colors)
        ) %>%
        plotly::layout(barmode = 'stack') %>%
        plotly::add_text(data = phase_times,
                         x =~ label_point,
                         text =~ paste0(name,": ",percent,"%"),
                         y = 500, textposition = "right",
                         textfont = list(size = 12, color = "#ffffff"))
})
    # }
  })
}


