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
mod_chart_roasting_profile_server <- function(id, json_filename) {
  moduleServer(id, function(input, output, session) {
    # print( length(json_filename))

    # json_filename = "Haiti--2021-02-09-20-15-17"
    # Read json file
      filename = paste0(".//data-raw/saved/", json_filename)

      json_file <-
        jsonlite::read_json(filename)

      # Get the deltas and clean them
      deltas = get_python_deltas(filename)
      cleaned_deltas = clean_deltas_from_python(deltas)

      times <-
        get_data_of_times_temps(json_file) %>%
        dplyr::mutate_if(is.character, as.numeric)

      # Deltas ready to be plotted, also has BT and ET
      deltas_clean = add_times_to_delta(cleaned_deltas, all_times = times)

  output$roast_profile <-
    plotly::renderPlotly({

    # Heat, fan times
  special_times <- get_special_event_times(json_file)

  # FC, phase times
  event_times <-
    get_event_times(json_file)

  # # This needs data from times
  # add_times_to_delta <-
  #   function(cleaned_deltas,
  #            time_list = times$time) {
  #     time_list = times$time # 472
  #     time_length = length(time_list)
  #     cleaned_deltas %>% dplyr::slice_tail(n = time_length) %>% dplyr::mutate(timex = time_list)
  #   }
  #
  # # Deltas ready to be plotted
  # deltas_clean = add_times_to_delta(cleaned_deltas)

  plotly::plot_ly(
    # BT Line
    times,
    type = 'scatter',
    mode = 'lines',
    x = ~ lubridate::as_datetime(time),
    line = list(color = "#4DB848"),
    # x = ~seq(ms("00:00"), ms("10:10")),
    # x = ~ lubridate::ms(Time2),
    # x = ~ lubridate::as_datetime(Time1),
    y = ~ BT,
    # hovertemplate = paste('%{y: .1f}\u00b0F', '<br>%{x}<br>'),
    hovertemplate = '%{y: .1f}\u00b0F',
    showlegend = FALSE,
    name = "BT"
  ) %>%
    plotly::add_trace(
      times,
      # ET Line
      mode = 'lines',
      x = ~ lubridate::as_datetime(time),
      y = ~ ET,
      line = list(color = "#D50032"),
      name = "ET"
    ) %>%
    plotly::layout(hovermode = "x unified") %>%
    plotly::add_annotations( # Special events
      data = special_times,
      x =  ~ lubridate::as_datetime(as.numeric(time_of_event)),
      # y = ~ jitter(400, 60),
      y = ~ 500,
      text = ~ type_of_event,
      # yaxis = "y2",
      textposition = "top center",
      showarrow = FALSE,
      # arrowhead = .5,
      # arrowwidth = 1,
      font = list(size = 12, color = "#ffffff"),
      bgcolor = ~ color
    ) %>%                     # Add lines for phases
    plotly::add_segments(
      x = ~ event_times$dry_time,
      xend = ~ event_times$dry_time,
      y =  ~ 0,
      yend =  ~ event_times$max_temp,
      # opacity = 1,
      line = list(
        dash = "dash",
        color = '#AAAAAA',
        width = 2
      ),
      name = "Dry end"
    ) %>%
    plotly::add_segments(
      x = ~ event_times$fc_time_start,
      xend = ~ event_times$fc_time_start,
      y =  ~ 0,
      yend =  ~ event_times$max_temp,
      # opacity = 1,
      line = list(
        dash = "dash",
        color = '#AAAAAA',
        width = 2
      ),
      name = "FC start"
    ) %>%
    plotly::add_segments(
      x = ~ event_times$fc_time_end,
      xend = ~ event_times$fc_time_end,
      y =  ~ 0,
      yend =  ~ event_times$max_temp,
      # opacity = 1,
      line = list(
        dash = "dash",
        color = '#AAAAAA',
        width = 2
      ),
      name = "FC end"
    ) %>%
    # For second_crash_start
    # add_segments( x = ~first_crack_start, xend = ~first_crack_start, y =~ 0, yend=~500,
    #               # opacity = 1,
    #               line = list(dash="dash",
    #                           color = 'gray80',
    #                           width = 2), name = "FC start") %>%
    plotly::add_segments(
      x = ~ event_times$drop_time,
      xend = ~ event_times$drop_time,
      y =  ~ 0,
      yend =  ~ event_times$max_temp,
      # opacity = 1,
      line = list(
        dash = "dash",
        color = '#AAAAAA',
        width = 2
      ),
      name = "FC start"
    ) %>%
    plotly::add_trace(
      data = deltas_clean,        # Change BT Line
      mode = 'lines',
      x = ~ lubridate::as_datetime(timex),
      y = ~ dtemp1,
      line = list(color = "#428BCA"),
      name = "\u0394ET",
      yaxis = "y2"
    ) %>%
    plotly::add_trace(
      data = deltas_clean,        # Change ET Line
      mode = 'lines',
      x = ~ lubridate::as_datetime(timex),
      y = ~ dtemp2,
      line = list(color = "#3f0585"),
      name = "\u0394BT",
      yaxis = "y2"
    ) %>%
    plotly::layout(
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
      xaxis = list(
        # gridcolor = toRGB("gray85"),
        title = "",
        zeroline = F,
        showline = F,
        showgrid = F,
        tick0 = event_times$time_zero,
        ticks = "inside",
        tickcolor = "rgb(245,245,245)",
        tickformat = "%M:%S",
        dtick = 30000 # Tick every 30 seconds
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
      plot_bgcolor = 'rgb(245,245,245)',
      # make grey background
      paper_bgcolor = 'rgb(245,245,245)'
    )
})
    # }
  })
}


