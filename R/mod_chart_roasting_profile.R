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
  ns <- shiny::NS(id)
  shiny::tagList(plotly::plotlyOutput(ns("roast_profile")))
}


#' chart_roasting_profile Server Function
#'
#' @noRd
#'
#'
mod_chart_roasting_profile_server <- function(id, profile) {
  shiny::moduleServer(id, function(input, output, session) {

  output$roast_profile <-
    plotly::renderPlotly({
    # Get data for certain parameters for chart
    time_zero = as_datetime("1970-01-01 00:00:00 UTC")
    time_max = max(as_datetime(profile$Time2), na.rm = TRUE)
    dry_end = as_datetime(profile$Time2[grepl("Dry End", profile$Event), "Time2"])
    first_crack_start = as_datetime(profile$Time2[grepl("FCs", profile$Event), "Time2"])
    first_crack_end = as_datetime(profile$Time2[grepl("FCe", profile$Event), "Time2"])
    drop_start = as_datetime(profile$Time2[grepl("Drop", profile$Event), "Time2"])
    max_temp = 500 # Highest temp in chart
    # print(profile$Time2)
      plotly::plot_ly(
      # BT Line
      profile,
      type = 'scatter',
      mode = 'lines',
      x = ~ lubridate::as_datetime(Time2),
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
      add_trace(         # ET Line
        mode = 'lines',
        x = ~ lubridate::as_datetime(Time2),
        y = ~ ET,
        line = list(color = "#D50032"),
        name = "ET"
      ) %>%
      add_trace(        # Change BT Line
        mode = 'lines',
        x = ~ lubridate::as_datetime(Time2),
        y = ~ change_BT,
        line = list(color = "#428BCA"),
        name = "\u0394BT",
        yaxis = "y2"
      ) %>% layout(hovermode = "x unified") %>%
      filter(!is.na(Event),
             !is.na(Time2),
             Event != "Drop",
             !grepl("^Charge", Event)) %>%
      add_annotations(
        # x =  ~lubridate::as_datetime(Time2), # jitter() ?
        # y = ~ jitter(BT, 60),
        text = ~ Event,
        # yaxis = "y2",
        textposition = "top center",
        arrowhead = .5,
        arrowwidth = 1,
        font = list(size = 12, color = "#ffffff"),
        bgcolor = ~ event_color
      ) %>%                     # Add lines for phases
      add_segments( x = ~dry_end, xend = ~dry_end, y =~ 0, yend=~500,
                    # opacity = 1,
                    line = list(dash="dash",
                                color = '#AAAAAA',
                                width = 2), text = "Dry end") %>%
      add_segments( x = ~first_crack_start, xend = ~first_crack_start, y =~ 0, yend=~500,
                    # opacity = 1,
                    line = list(dash="dash",
                                color = '#AAAAAA',
                                width = 2), text = "FC start") %>%
      add_segments( x = ~first_crack_end, xend = ~first_crack_end, y =~ 0, yend=~500,
                    # opacity = 1,
                    line = list(dash="dash",
                                color = '#AAAAAA',
                                width = 2), text = "FC start") %>%
      # For second_crash_start
      # add_segments( x = ~first_crack_start, xend = ~first_crack_start, y =~ 0, yend=~500,
      #               # opacity = 1,
      #               line = list(dash="dash",
      #                           color = 'gray80',
      #                           width = 2), text = "FC start") %>%
      layout(
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
          tick0 = time_zero,
          ticks = "inside",
          tickcolor = "grey80",
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
})
}


#' chart_roasting_profile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_chart_roasting_profile_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' chart_roasting_profile Server Function
#'
#' @noRd 
mod_chart_roasting_profile_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_chart_roasting_profile_ui("chart_roasting_profile_ui_1")
    
## To be copied in the server
# callModule(mod_chart_roasting_profile_server, "chart_roasting_profile_ui_1")
 
