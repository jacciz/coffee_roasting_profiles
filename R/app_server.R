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



  #  -------------- Compare multiple profiles -------------------
  many_profiles <- reactive({

    # If false does not exist in string
    if ("FALSE" %in% as.character(
      lapply(input$roast_curves_upload_multiple$name,is_file_valid)) == FALSE) {
      profiles <- lapply(input$roast_curves_upload_multiple$datapath,
                         open_profile_as_json)
      cleaned <-
        do.call(dplyr::bind_rows, lapply(profiles, clean_raw_alog))

      cleaned_cross <- crosstalk::SharedData$new(cleaned)
      cleaned_cross

    } else if (!is.null(input$roast_curves_upload_multiple$name) | "FALSE" %in%
               as.character(lapply(
                 input$roast_curves_upload_multiple$name,is_file_valid))) {
      print("invalid")
      showModal(modalDialog(
        title = "Error2",
        paste("At least one invalid .alog"),
        easyClose = TRUE
      ))
    }
  })

  observeEvent(input$roast_curves_upload_multiple$datapath, {
    # print( "FALSE" %in%
    #          as.character(lapply(
    #            input$roast_curves_upload_multiple$name,is_file_valid)))
    req("FALSE" %in% as.character(
      lapply(input$roast_curves_upload_multiple$name,is_file_valid)) == FALSE)

      # Cross talk it!
      profiles <- many_profiles()$data(withSelection = TRUE)

      DT_table <- get_DT_table(profiles)
      mod_chart_roasting_profile_multiple_server("uploaded_profile_multiple", profiles)
      mod_DT_summary_table_server("summary_table_multiple", DT_table)

  })

  #  -------------- Examine single profile --------------------
  one_profile <- reactive({
    if (is_file_valid(input$roast_curves_upload_single)) {
      profile <-
        open_profile_as_json(alog_input = input$roast_curves_upload_single$datapath)
      return(clean_raw_alog(profile))
    } else if (!is.null(input$roast_curves_upload_single)) {
      # TODO feedback doesn't work
      shinyFeedback::feedbackWarning(
        "roast_curves_upload",
        !is_file_valid(input$roast_curves_upload_single),
        text = "Input .alog"
      )
      showModal(modalDialog(
        title = "Error",
        paste("Upload valid .alog"),
        easyClose = TRUE
      ))
    }
  })

  observeEvent(one_profile(), {
    # Require these to be valid. If they are all valid, code will finally proceed.
    req(is_file_valid(input$roast_curves_upload_single))
      display_this <- get_data_to_display_at_upload(one_profile()) %>%
        tidyr::pivot_longer(cols = 1:4, values_to = "data")
      DT_table <- get_DT_table(one_profile())
      mod_chart_summary_table_template_server("uploaded_data_preview_single", display_this)
      mod_chart_roasting_profile_server("uploaded_profile_single", one_profile())
      mod_DT_summary_table_server("summary_table_single", DT_table)
  })

}
