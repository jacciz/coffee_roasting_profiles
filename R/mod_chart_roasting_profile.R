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
      # deltas = get_python_deltas(filename)
      # cleaned_deltas = clean_deltas_from_python(deltas)

      times <-
        get_data_of_times_temps(json_file) %>%
        dplyr::mutate_if(is.character, as.numeric)

      # Deltas ready to be plotted, also has BT and ET
      # deltas_clean = add_times_to_delta(cleaned_deltas, all_times = times)

      phase_times = get_data_of_phase_times(json_file) %>%
        tidyr::pivot_longer(everything(), values_to = "phase_time") %>%
        dplyr::mutate(name = factor(.data$name, levels = c("Dry", "Mid", "Dev"))) %>% # add levels
        dplyr::mutate(percent = as.character(round(.data$phase_time * 100 / sum(.data$phase_time),1)), # Get phase percent
                      phase_time_mid = cumsum(.data$phase_time)) %>% # Get midpoint
        dplyr::mutate_if(is.integer, lubridate::as_datetime) %>% # Make into datetime so we can chart it
        dplyr::mutate_if(is.numeric, lubridate::as_datetime)

      # Get start times of each phase so that is where the label will be
      phase_times[phase_times$name == "Dry", "label_point" ] = as_datetime(0)
      phase_times[phase_times$name == "Mid", "label_point" ] = phase_times[phase_times$name == "Dry", "phase_time_mid" ] + 1
      phase_times[phase_times$name == "Dev", "label_point" ] = phase_times[phase_times$name == "Mid", "phase_time_mid" ] + 1

    # colors for the 3 phase bars on top
      phase_colors <- c('#4DB848', '#FF9673', '#E1C8B4')

  output$roast_profile <-
    plotly::renderPlotly({

    # Heat, fan times
  special_times <- get_special_event_times(json_file)

  # FC, phase times
  event_times <-
    get_event_times(json_file)

  plotly::plot_ly(
    data = times,
    x = ~ lubridate::as_datetime(time)
  ) %>%
    plotly::add_lines(
      data = times,  # ET Line
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
      data = times,  # ET Line
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
    plotly::add_annotations( # Special events
      data = special_times,
      x =  ~ lubridate::as_datetime(as.numeric(time_of_event)),
      # y = ~ jitter(400, 60),
      y = ~ 10,
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
      showlegend = FALSE,
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
      showlegend = FALSE,
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
      showlegend = FALSE,
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
   plotly:: add_segments(
     x = ~event_times$sc_time_start,
    xend = ~event_times$sc_time_start,
    y =~ 0,
    showlegend = FALSE,
    yend=~event_times$max_temp,
                  # opacity = 1,
                  line = list(dash="dash",
                              color = '#AAAAAA',
                              width = 2), name = "FC start") %>%
    plotly::add_segments(
      x = ~ event_times$drop_time,
      xend = ~ event_times$drop_time,
      showlegend = FALSE,
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
    # plotly::add_trace(
    #   data = deltas_clean,        # Change BT Line
    #   mode = 'lines',
    #   x = ~ lubridate::as_datetime(timex),
    #   y = ~ dtemp1,
    #   line = list(color = "#428BCA"),
    #   name = "\u0394ET",
    #   yaxis = "y2"
    # ) %>%
    # plotly::add_trace(
    #   data = deltas_clean,        # Change ET Line
    #   mode = 'lines',
    #   x = ~ lubridate::as_datetime(timex),
    #   y = ~ dtemp2,
    #   line = list(color = "#3f0585"),
    #   name = "\u0394BT",
    #   yaxis = "y2"
    # ) %>%
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
    plotly::add_text(data = phase_times, x =~ label_point, text =~ paste0(name,": ",percent,"%"),y = 500, textposition = "right", textfont = list(size = 12, color = "#ffffff"))
})
    # }
  })
}


