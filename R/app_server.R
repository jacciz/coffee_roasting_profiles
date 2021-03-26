#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny lubridate plotly formattable dplyr stringr tidyr
#' @noRd
app_server <- function( input, output, session ) {

    # ------------------------- Reactives  --------------------------------------
  #
  # get_selected_profile <- # Based on country for now
  #     reactive({
  #         dbReadTable(pool, "roast_profiles") %>% filter(country == input$country)# filter(Org_Name %in% input$health_clinic)
  #     })
  valid_filename <-
    reactive({
      if (base::exists("input$selected_filename")) {
        input$selected_filename
      } else {
        # NULL
        "Angola--2021-03-05-18-06-43.json" # open this as default
      }

    })
  get_profile_database <- # Based on country for now
      reactive({
          pool::dbReadTable(pool, "roast_profiles") #%>% filter(country == input$country)# filter(Org_Name %in% input$health_clinic)
      })

  # Read the selected file, opens sql and json
  open_profile_by_filename <-
    reactive({
      pool::dbReadTable(pool, "roast_profiles") %>% filter(filename == valid_filename())
    })

  open_profile_by_filename_json <-
    reactive({
        filename = paste0(".//data-raw/saved/", valid_filename())
        json_file <- jsonlite::read_json(filename)
        json_file
    })

  #  -------------- The Roast Profile Analysis Line Chart --------------------
 observeEvent(input$selected_filename, {
   # if (length(input$selected_filename) > 5 | input$selected_filename != "") {
  mod_chart_roasting_profile_server("roast_profile_chart", valid_filename())# open_profile_by_filename()
     # }
     })

  # roasting_profile_data <- reactive({
  #   # if (length(input$selected_filename) > 5 | input$selected_filename != "" | !is.null(input$selected_filename)) {
  #   open_profile_by_filename_json() %>%
  #       get_event_times() %>%
  #       # select(.data$fc_time_start, .data$sc_time_start, .data$drop_time) %>%
  #       select(`First Crack` = .data$fc_time_start, `Second crack` = .data$sc_time_start, `Roast time` = .data$drop_time) %>%
  #       mutate_if(is.POSIXct, format, format = "%M:%S") %>%
  #       pivot_longer(cols = everything(), values_to = "data")
  # })
  #  -------------- Gather data for gt tables --------------------
  examine_profile_summary_table <- reactive({
  open_profile_by_filename() %>%
    select(-.data$filename,
           -.data$primary_key) %>%
    select(`Date uploaded` = .data$date_uploaded,
           `Roaster name` = .data$name,
           `Roast machine` = .data$roast_machine,
           `Farm` = .data$roast_farm,
           Country = .data$country,
           Region = .data$region,
           `Quality/Grade` = .data$quality,
           `Processing method` = .data$processing_method,
           Variety = .data$variety
    ) %>%
    pivot_longer(cols = everything(), values_to = "data")
  })
  #  -------------- Mods for gt tables --------------------
  mod_chart_summary_table_template_server("roasting_db_data", examine_profile_summary_table())


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
  # bucketlistlabels_tasting()
  # output$click <- renderText(click())
  output$bucketlist_tasting <- renderUI({
  sortable::bucket_list(
    header = "",
    group_name = "bucket_list_tasting",
    orientation = "horizontal",
    sortable::add_rank_list(
      text = "",
      # labels = bucketlistlabels_tasting(),
      labels = r$click, # must be added one at a time
      input_id = "rank_list_tasting"
      # options = sortable::sortable_options(
      #   onLoad = htmlwidgets::JS("function (evt) {this.el.addChild(evt.item);}"),
      #   onSort = htmlwidgets::JS("function (evt) {this.el.addChild(evt.item);}")
      # )
    ),
    sortable::add_rank_list(
      text = "Drag to Remove",
      labels = NULL,
      input_id = "remove_tasting",
      options = sortable::sortable_options(
        onAdd = htmlwidgets::JS("function (evt) {this.el.removeChild(evt.item);}")
      )
    )
  )
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

  output$get_filenames_saved <- renderUI({
    uploaded_filenames <- get_profile_database()$filename #%>% rev()
    selectInput("selected_filename", "Choose file", uploaded_filenames, width = '100%', selected = NULL)
  })
  output$get_data_upload_date <- renderUI({
    unique_upload_dates <- get_profile_database()$date_uploaded %>% unique()
    selectInput("selected_upload_date", "Upload date", unique_upload_dates, width = '100%', selected = NULL)
  })
  # Make dropdown menu of all countries, roast machines,
  output$get_country <- renderUI({
    countries <- get_profile_database()$country %>% unique()
    pickerInput("selected_country", "Country", countries, width = '100%', selected = NULL)
  })
  # output$get_roast_machine <- renderUI({
  #   machines <- coffee_producting_countries %>% sort()
  #   pickerInput("roast_machine", "Machine", coffee_roasting_machines, width = '100%', selected = NULL)
  # })
  output$get_processing_method <- renderUI({
    method <- get_profile_database()$processing_method %>% unique()
    pickerInput("selected_processing_method", "Processing method", method, width = '100%', selected = NULL)
  })
  # output$get_variety <- renderUI({
  #   varieties <- get_profile_database()$date_uploaded %>% unique()
  #   pickerInput("variety", "Variety", varieties, width = '100%', selected = NULL)
  # })

  # get text from roast update tab
  # output$value <- renderPrint(input$roast_machine)
  #  -------------- Input Roast Data from file/user --------------------

  # Define fields we want to save on the form, this is based on the textInput IDs
  fields_to_update_for_profile <-
    c(
        # "primary_key",
        "name",
        "roast_machine",
        "roast_farm",
        "country",
        "region",
        "quality",
        "processing_method"
        # "variety" this is saved later since it is a list, must be converted to string
        )

  # Collect the form data and save it into the "data" list variable
  form_data_contact_db <- reactive({
    # print( input[[contact_register_fields[1]]]) # works
    data <- sapply(fields_to_update_for_profile, function(x) input[[x]] ) # fields contains all values we want to save, gather all the values based on input
    data <- Filter(function(x) !(all(x == "" | x == 0)), data) # take out 0 or empty values
    data
  })

  # This checks to see if uploaded profile is valid, returns booleon
  valid_profile_upload <- reactive({
    if (any(c(
      length(input$roast_curves_upload$datapath) != 0 &
      !is.null(input$roast_curves_upload) &
      str_sub(input$roast_curves_upload$datapath, -5, -1) == ".alog"
    ))) {
      return(TRUE)
    }
    return(FALSE)
  })

  # When submit button is pushed, save the form data - it doesn't change the data - just saves it ??
  observeEvent(input$save_record, {

    feedbackWarning("variety", is.null(input$variety), "Enter variety")

    # Conditions we want all to be true. Returns true if all are met.
    is_file_valid <- any(c(length(input$roast_curves_upload$datapath) != 0 &
                             !is.null(input$roast_curves_upload) &
                             str_sub(input$roast_curves_upload$datapath, -5, -1) == ".alog")) # Input file must end in .alog

    feedbackWarning(
      "roast_curves_upload", !is_file_valid,
      "Input .alog"
    )

    # And then require these to be valid. If they are all valid, code will finally proceed.
    req(
      # is.numeric(input$weight_before) & input$weight_before > 0,
      # is.numeric(input$weight_after) & input$weight_after > 0,
      !is.null(input$variety),
      is_file_valid
    )

    # Then open alog, returns a long string
    opened_json <- open_profile_as_json(alog_input = input$roast_curves_upload$datapath)

    # Convert opened JSON into R format :)
    profile_as_json <- jsonlite::fromJSON(opened_json)
    # print(profile_as_json) works
    # Get the filename so we can save and store filename
    # Returns filename (i.e.filename.json)
    saved_filename <- get_profile_filename(profile_as_json,
                                           country = input$country,
                                           region = input$region)

    # Saves opened JSON profile in the folder. Cannot be an R object.
    save_profile_json(profile_as_json, saved_filename)

    # This saves the SQL db part
    # Make the filename list so we can append to all_inputs_to_save
    save_filename_list <- c("filename" = saved_filename) %>% as.list()

    # Get date of upload
    save_upload_date <- c("date_uploaded" = as.character(Sys.Date())) %>% as.list()

    # Variety is saved as a list, must convert to a string and put string into a list to save to db
    variety_as_char <-
      as.character(input$variety) %>% stringr::str_flatten(., collapse = ", ")
    save_variety <- c("variety" = variety_as_char) %>% as.list()

    # Combine variety and filename to all inputs (which form_data_contact_db runs) in order to dump them in db under 1 sql query
    # These all must be in fields_to_update_for_profile in order to save
    all_inputs_to_save <-
      unlist(c(save_upload_date, save_variety, save_filename_list, form_data_contact_db()))

    # Then save all the inputs!
    record_status <-
      update_roast_profiles(all_inputs_to_save, "roast_profiles") # name of db table

    showModal(modalDialog(
      title = "Update Success",
      paste("Record was ", record_status),
      easyClose = TRUE
    ))
  })

  # When the new button is pushed, need session object so clear out fields
 observeEvent(input$roast_curves_upload$datapath, {
    if (!is.null(input$roast_curves_upload)) {
          opened_json <-
            open_profile_as_json(alog_input = input$roast_curves_upload$datapath)

          # Convert opened JSON into R format :)
          profile_as_json <-
            jsonlite::fromJSON(opened_json)

          upload_this <- get_data_to_display_at_upload(profile_as_json) %>%
            tidyr::pivot_longer(cols = 1:7, values_to = "data")
    mod_chart_summary_table_template_server("uploaded_data_preview", upload_this)
    }

  })

}
