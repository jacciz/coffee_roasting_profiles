#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny lubridate plotly formattable dplyr stringr tidyr
#' @noRd
app_server <- function( input, output, session ) {

    # ------------------------- Reactives  --------------------------------------

    #  -------------- The Roast Profile Analysis Line Chart --------------------

  #  -------------- Gather data for gt tables --------------------
  # examine_profile_summary_table <- reactive({
  # open_profile_by_filename() %>%
  #   select(-.data$filename,
  #          -.data$primary_key) %>%
  #   select(`Date uploaded` = .data$date_uploaded,
  #          `Roaster name` = .data$name,
  #          `Roast machine` = .data$roast_machine,
  #          `Farm` = .data$roast_farm,
  #          Country = .data$country,
  #          Region = .data$region,
  #          `Quality/Grade` = .data$quality,
  #          `Processing method` = .data$processing_method,
  #          Variety = .data$variety
  #   ) %>%
  #   pivot_longer(cols = everything(), values_to = "data")
  # })
  # #  -------------- Mods for gt tables --------------------
  # mod_chart_summary_table_template_server("roasting_db_data", examine_profile_summary_table())


  #  -------------- The Roast Profile --------------------

  #  -------------- The Roast Profile datatable --------------------
  output$hover <- renderText({
    input$hover_data
  })
  output$click <- renderText({
    # input$click_data
    r$click
  })

  # Accumulate flavor notes when clicked - https://mastering-shiny.org/reactivity-components.html
  # flavor <- reactiveValues(names = character())
  # observeEvent(input$click_data, {
  #   flavor$names <- c(input$click_data, flavor$names)
  #   updateTextInput(session, "notes", value = "")
  # })
  # output$names <- renderText(names())

  # input = click_data  output = click
  r <- reactiveValues(click = character())
  observeEvent(input$click_data, {
    # print(input$click_data) # works
    # r$click <- union(input$click_data, r$click) # union so there's no duplicates
    r$click <- c(input$click_data, r$click) # want duplicates in case one gets deleted,
    # but can't click same twice in a row
    # r$click <- input$click_data
    # print(r$click)
  })
  #  -------------- Coffee Cupping Sunburst --------------------
  output$coffee_tasting <- renderPlotly2({
    base::get("coffee_cupping_tasting")
    coffee_cupping_tasting <- coffee_cupping_tasting %>% mutate(new_label = paste0("<b>", .data$end_name,": ", "</b>",stringr::str_wrap(labels, width = 30)))
    # coffee_tasting$name = rownames(coffee_tasting)
    coffee_cupping_tasting %>% plot_ly() %>% add_trace(
      type = 'sunburst',
      ids = coffee_cupping_tasting$ids,
      labels = coffee_cupping_tasting$end_name,
      parents = coffee_cupping_tasting$parents,
      text = ~coffee_cupping_tasting$new_label,
      textinfo = 'label',  # What shows on the chart
      domain = list(column = 1),
      maxdepth = 3,
      insidetextorientation = 'radial'
    ) %>%
      layout(# grid = list(columns =2, rows = 1),
        margin = list(
          l = 0,
          r = 0,
          b = 0,
          t = 0
        )) %>%
      # as_widget(p) %>% #?
      htmlwidgets::onRender(addClickBehavior)# %>%
      # htmlwidgets::onRender(disable_sunburst_animation)
      #onRender(addHoverBehavior)
  })
  #  -------------- Coffee Tasting Sunburst --------------------
  output$coffee_flavors <- renderPlotly({
    base::get("coffee_flavors")
    coffee_flavors %>% plot_ly() %>% add_trace(
      type = 'sunburst',
      ids = coffee_flavors$ids,
      labels = coffee_flavors$labels,
      parents = coffee_flavors$parents,
      domain = list(column = 1),
      maxdepth = 3,
      insidetextorientation = 'radial'
    ) %>% layout(# grid = list(columns =2, rows = 1),
      margin = list(
        l = 0,
        r = 0,
        b = 0,
        t = 0
      ))
    # sunburstcolorway = c(
    #     "#636efa","#EF553B","#00cc96","#ab63fa","#19d3f3",
    #     "#e763fa", "#FECB52","#FFA15A","#FF6692","#B6E880"
    # ),
    # extendsunburstcolors = TRUE) # This makes colors light with more slices
  })

    #  -------------- Input Roast Data from file/user --------------------
  # Example: https://www.youtube.com/user/RodCoelho/search?query=MySQL Don't need loadDropdown as we are not deleting records
  #  -------------- Input Roast Data - Dropdowns for Form --------------------

  # This checks to see if uploaded profile is valid, returns booleon
  # valid_profile_upload <- reactive({
  #   if (any(c(
  #     length(input$roast_curves_upload$datapath) != 0 &
  #     !is.null(input$roast_curves_upload) &
  #     str_sub(input$roast_curves_upload$datapath, -5, -1) == ".alog"
  #   ))) {
  #     return(TRUE)
  #   }
  #   return(FALSE)
  # })

# Conditions we want all to be true. Returns true if all are met.
    # is_file_valid <- any(c(length(input$roast_curves_upload$datapath) != 0 &
    #                          !is.null(input$roast_curves_upload) &
    #                          str_sub(input$roast_curves_upload$datapath, -5, -1) == ".alog")) # Input file must end in .alog
    #
    # feedbackWarning(
    #   "roast_curves_upload", !is_file_valid,
    #   "Input .alog"
    # )

    # df <- debounce(reactive(shared_iris$data(withSelection = TRUE)), 250)
  #  -------------- Compare multiple profiles -------------------
  many_profiles <- reactive({
    profile <- open_profile_as_json(
      alog_input = input$roast_curves_upload_multiple$datapath)
    cleaned <- clean_raw_alog(profile)
    cleaned_cross <- crosstalk::SharedData$new(cleaned)
    cleaned_cross
  })

  # When the new button is pushed, need session object so clear out fields
  observeEvent(input$roast_curves_upload_multiple$datapath, {
    if (!is.null(input$roast_curves_upload_multiple)) {

      # Cross talk it!
      one_profile <- many_profiles()$data(withSelection = TRUE)
      display_this <- get_data_to_display_at_upload(one_profile) %>%
        tidyr::pivot_longer(cols = 1:4, values_to = "data")
      #
      DT_table <- get_DT_table(one_profile)
      mod_chart_summary_table_template_server("uploaded_data_preview", display_this)
      mod_chart_roasting_profile_server("uploaded_profile", one_profile)
      mod_DT_summary_table_server("summary_table", DT_table)
    }
  })

  #  -------------- Examine single profiles --------------------
  one_profile <- reactive({
    profile <- open_profile_as_json(
      alog_input = input$roast_curves_upload_single$datapath)
    clean_raw_alog(profile)
  })

  observeEvent(input$roast_curves_upload_single$datapath, {
    if (!is.null(input$roast_curves_upload_single)) {

      display_this <- get_data_to_display_at_upload(one_profile()) %>%
        tidyr::pivot_longer(cols = 1:4, values_to = "data")
      DT_table <- get_DT_table(one_profile())
      mod_chart_summary_table_template_server("uploaded_data_preview_single", display_this)
      mod_chart_roasting_profile_server("uploaded_profile_single", one_profile())
      mod_DT_summary_table_server("summary_table_single", DT_table)
    }
  })

}
