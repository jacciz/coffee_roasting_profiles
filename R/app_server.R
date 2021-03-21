#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny lubridate plotly formattable dplyr stringr tidyr
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here

    # ------------------------- Reactives  --------------------------------------
  #
  # get_selected_profile <- # Based on country for now
  #     reactive({
  #         dbReadTable(pool, "roast_profiles") %>% filter(country == input$country)# filter(Org_Name %in% input$health_clinic)
  #     })

  get_profile_database <- # Based on country for now
      reactive({
          pool::dbReadTable(pool, "roast_profiles") #%>% filter(country == input$country)# filter(Org_Name %in% input$health_clinic)
      })

  # Read the selected file, opens sql and json
  open_profile_by_filename <-
    reactive({
      if (length(input$selected_filename) > 5 | input$selected_filename != "") {
      pool::dbReadTable(pool, "roast_profiles") %>% filter(filename == input$selected_filename)
      }
    })

  open_profile_by_filename_json <-
    reactive({
      if (length(input$selected_filename) > 5 | input$selected_filename != "") {
        filename = paste0(".//data-raw/saved/", input$selected_filename)
        json_file <- jsonlite::read_json(filename)
        json_file
      }
    })

  #  -------------- The Roast Profile Analysis Line Chart --------------------
 observeEvent(input$selected_filename, {
   if (length(input$selected_filename) > 5 | input$selected_filename!= "") {
  mod_chart_roasting_profile_server("roast_profile_chart", input$selected_filename )# open_profile_by_filename()

     }
     })
  # output$summary_profile_data <- function(){
  # input$uploaded_filenames is the filename
  # }
  output$roasting_profile_data <- renderFormattable({
    if (length(input$selected_filename) > 5 | input$selected_filename != "" | !is.null(input$selected_filename)) {
    open_profile_by_filename_json() %>%
        get_event_times() %>%
        select(fc_time_start, sc_time_start, drop_time) %>%
        select(`First Crack` = fc_time_start, `Second crack` = sc_time_start, `Roast time` = drop_time) %>%
        mutate_if(is.POSIXct, format, format = "%M:%S") %>%
        pivot_longer(cols = everything(), values_to = "data") %>%
        formattable::formattable(.,
                                 list(
                                   name = formattable::formatter("span", style = "color:#AAAAAA; font-size:.8vw; font-weight:bold;border-color:#fff"),
                                   data = formattable::formatter("span", style = "color:#AAAAAA;font-size:.8vw;border-color:#fff")
                                 ))
    } else {
      formattable::formattable()
    }
  })


  output$roasting_db_data <- gt::render_gt({
    if (length(input$selected_filename) > 5 |
        input$selected_filename != "" |
        !is.null(input$selected_filename)) {
      open_profile_by_filename() %>%
        select(-filename,
               -primary_key,
               -roast_date,
               -unit_of_measure,
               -cupping_notes,
               -aromatic_notes,
               -weight_before,
               -weight_after,
               -roast_notes,
               -taste_notes) %>%
        select(`Date uploaded` = date_uploaded,
               `Roaster name` = name,
               `Roast machine` = roast_machine,
               `Farm` = roast_farm,
               Country = country,
               Region = region,
               `Quality/Grade` = quality,
               `Processing method` = processing_method,
               Variety = variety
               ) %>%
        # mutate(`Weight loss` = ifelse(is.na(weight_before), 0, round((
        #   weight_before * 100 / weight_after
        # ), 1))) %>%
        # mutate_if(is.integer, as.character) %>%
        # mutate_if(is.double, as.character) %>%
        # select(-weight_before, -weight_after) %>%
        pivot_longer(cols = everything(), values_to = "data") %>%
        gt::gt() %>%
        gt::tab_style(
          style = list(
            gt::cell_borders(
              sides = "all",
              color = "white",
              style = "solid",
              weight = gt::px(0)
            ),
            gt::cell_text(font = "Verdana", size = "0.8vw")
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
    } else {
      gt::gt()
    }
  })
  # output$roasting_db_data <- renderFormattable({
  #   if (length(input$selected_filename) > 5 | input$selected_filename != "" | !is.null(input$selected_filename)) {
  #     open_profile_by_filename() %>%
  #       select(-filename, -primary_key) %>%
  #       mutate(`Weight loss` = ifelse(is.na(weight_before), 0, round((weight_before*100/weight_after),1))) %>%
  #       mutate_if(is.integer, as.character) %>%
  #       mutate_if(is.double, as.character) %>%
  #       select(-weight_before, - weight_after) %>%
  #       pivot_longer(cols = everything(), values_to = "data") %>%
  #       formattable::formattable(.,
  #                                list(
  #                                  name = formattable::formatter("span", style = "color:#AAAAAA; font-size:14px; font-weight:bold;border-color:#fff"),
  #                                  data = formattable::formatter("span", style = "color:#AAAAAA;font-size:14px;border-color:#fff")
  #                                ))
  #   }
  #   else {
  #     formattable::formattable()
  #   }
  # })
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
    print(r$click)
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
    coffee_cupping_tasting <- coffee_cupping_tasting %>% mutate(new_label =paste0("<b>",end_name,": ", "</b>",stringr::str_wrap(labels, width = 30)))
    # coffee_tasting$name = rownames(coffee_tasting)
    coffee_cupping_tasting %>% plot_ly() %>% add_trace(
      type = 'sunburst',
      ids = coffee_cupping_tasting$ids,
      labels = coffee_cupping_tasting$end_name,
      parents = coffee_cupping_tasting$parents,
      text = ~coffee_cupping_tasting$new_label,
      textinfo = 'label',  # What shows on the chart
      hovertemplate = '%{text}',
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
  output$value <- renderPrint(input$roast_machine)
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
        # "roast_notes"
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

    # After button is pushed, check for these validations, if not satisfied a message will pop
    # feedbackWarning(
    #   "weight_before",
    #   !is.numeric(input$weight_before) |
    #     input$weight_before <= 0,
    #   "Enter valid number"
    # )
    # feedbackWarning(
    #   "weight_after",
    #   !is.numeric(input$weight_after) |
    #     input$weight_after <= 0,
    #   "Enter valid number"
    # )

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
    # sub <- input$roast_curves_upload$datapath
    # print(sub)
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
    tosave <- jsonlite::toJSON(profile_as_json)

    write(tosave, paste0("data-raw/saved/",saved_filename))

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
      upload_roast_profiles(all_inputs_to_save, "roast_profiles") # name of db table

    showModal(modalDialog(
      title = "Update Success",
      paste("Record was ", record_status),
      easyClose = TRUE
    ))
  })

  # When the new button is pushed, need session object so clear out fields
  # update_profile_fields <- function(profile_data) {
  #   profile <- profile_data
  # }

  output$uploaded_data_preview <- function() {
    if (valid_profile_upload()) {
      opened_json <-
        open_profile_as_json(alog_input = input$roast_curves_upload$datapath)
      # message(input$roast_curves_upload$datapath)

      # Convert opened JSON into R format :)
      profile_as_json <-
        jsonlite::fromJSON(opened_json)

      get_data_to_display_at_upload(profile_as_json) %>%
        tidyr::pivot_longer(cols = 1:7, values_to = "data") %>%
        kableExtra::kbl(col.names = c("", ""), align = "l", centering = FALSE) %>%
        kableExtra::kable_minimal()
    } else{
      return(NULL)
    }
  }
}
