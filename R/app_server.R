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
  open_profile_by_filename <- # Based on country for now
    reactive({
      pool::dbReadTable(pool, "roast_profiles") %>% filter(filename == input$selected_filename)
    })


  #  -------------- The Roast Profile Analysis Line Chart --------------------
 observeEvent(input$selected_filename, {
  mod_chart_roasting_profile_server("roast_profile_chart", input$selected_filename )# open_profile_by_filename()
 })
  # output$summary_profile_data <- function(){
  # input$uploaded_filenames is the filename
  # }
  output$roasting_profile_data <- renderFormattable({

    get_roasting_profile_data <- function(df) {
      chart_analysis <- c(
        time_zero = "1970-01-01 00:00:00 UTC",
        charge_start = format(as_datetime(df$Time2[grepl("Charge", df$Event), "Time2"]), format = "%M:%S"),
        time_max = format(max(as_datetime(df$Time2), na.rm = TRUE), format = "%M:%S"),
        dry_end = format(as_datetime(df$Time2[grepl("Dry End", df$Event), "Time2"]), format = "%M:%S"),
        first_crack_start = format(as_datetime(df$Time2[grepl("FCs", df$Event), "Time2"]), format =  "%M:%S"),
        first_crack_end = format(as_datetime(df$Time2[grepl("FCe", df$Event), "Time2"]), format = "%M:%S"),
        drop_start = format(as_datetime(df$Time2[grepl("Drop", df$Event), "Time2"]), format = "%M:%S"),
        # max_temp = 500, # Highest temp in chart
        end_temp = as.character(as.numeric(df[grepl("Drop", df$Event), "BT"]))
      ) %>% as.list()

      chart_analysis
    }
    chart_analysis <- get_roasting_profile_data(haiti)

    # Do calculations, finalize numbers and time to make into a table
    chart_analysis <-
      chart_analysis %>% as.data.frame() %>% mutate(
        duration = ms(drop_start) - ms(charge_start),
        dev_time = ms(drop_start) - ms(first_crack_start),
        duration = format(as_datetime(duration), format = "%M:%S"),
        dev_ratio = paste0(round(ms(dev_time) * 100 / ms(duration), 1), "%"),
        dev_time = format(as_datetime(dev_time), format = "%M:%S"),
        end_temp = round(as.numeric(end_temp), 1)
      ) %>% mutate_all(as.character) %>% select(
        "First crack" = first_crack_start,
        "Roast duration" = duration,
        "Development time" = dev_time,
        "Dev. time ratio" = dev_ratio,
        "End temp.(\u00b0F)" = end_temp
        # "End temp." = paste0(end_temp, "\u00b0F")
      )

    # make a datatable for output
    chart_analysis %>% pivot_longer(cols = 1:5, values_to = "data") %>% formattable::formattable(., list(
      name = formattable::formatter("span", style = "color:#AAAAAA; font-size:14px; font-weight:bold;"),
      data = formattable::formatter("span", style = "color:grey;")
    ))
  })
  #  -------------- The Roast Profile --------------------

  #  -------------- The Roast Profile datatable --------------------
  output$hover <- renderText({
    input$hover_data
  })
  output$click <- renderText({
    input$click_data
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
    r$click <- union(input$click_data, r$click)
    # print(r$click)# works
    # updateTextInput(session, "click_data", value = "")
  })

  # output$click <- renderText(click())


  #  -------------- Coffee Cupping Sunburst --------------------
  output$coffee_tasting <- renderPlotly2({
    coffee_cupping_tasting <- coffee_cupping_tasting %>% mutate(new_label =paste0("<b>",end_name,": ", "</b>",stringr::str_wrap(labels, width = 30)))
    # coffee_tasting$name = rownames(coffee_tasting)
    p <- coffee_cupping_tasting %>% plot_ly() %>% add_trace(
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
      as_widget(p) %>%
      htmlwidgets::onRender(addClickBehavior)#onRender(addHoverBehavior)
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

  #  -------------- Chart of QCI --------------------
  # output$arabica <- renderDT({
  #     datatable(arab)
  # })

  #  -------------- Input Roast Data from file/user --------------------
  # Example: https://www.youtube.com/user/RodCoelho/search?query=MySQL Don't need loadDropdown as we are not deleting records

  # # name
  # list_data <- loadData(c("primary_key", "roast_date", "country"), "roast_profiles") %>% as.data.frame()
  # list_data <- rbind(data.frame("primary_key" = 0, "roast_date" = Sys.Date(), "country" = "Other"),list_data) # Adds a new row of empty values
  # # country_list <- setNames(nm = c(list_data$primary_key, list_data$roast_data, list_data$country)) # strips the header names
  # names(list_data) <- NULL
  # # output$get_primary_key <- renderUI({
  # #     selectInput("selected_primary_key", "primary Keys", country_list, width = '100%', selected = NULL)
  # # })
  #
  # list_data <- loadData(c("roast_date", "country"), "roast_profiles") %>% as.data.frame()
  # list_data <- rbind(data.frame("roast_date" = Sys.Date(), "country" = "None"),list_data) # Adds a new row of empty values
  # # country_list <- setNames(nm = c( list_data$roast_data, list_data$country)) # strips the header names
  # names(list_data) <- NULL # Strip header
  # output$get_primary_key <- renderUI({
  #     selectInput("selected_primary_key", "primary Keys", country_list, width = '100%', selected = NULL)
  # })


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
        # "name",
        "roast_machine",
        "roast_farm",
        "country",
        "region",
        "quality",
        "processing_method",
        "roast_notes"
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
      # x <- profile_as_json$timex %>% as.data.frame() %>% head()
      # print(x)
      get_data_to_display_at_upload(profile_as_json) %>%
        tidyr::pivot_longer(cols = 1:7, values_to = "data") %>%
        kableExtra::kbl(col.names = c("", ""), align = "l", centering = FALSE) %>%
        kableExtra::kable_minimal()
      # formattable(x)
    } else{
      return(NULL)
    }
  }
}
