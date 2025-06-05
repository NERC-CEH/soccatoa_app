#' modal_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_modal_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # use js script
    includeScript("inst/app/www/script.js"),

    # the rest
    h5("Upload your dataset in our database"),
    br(),

    # bar to load the file
    fluidRow(
      column(
        6,
        fileInput(ns("upload"), label = NULL, buttonLabel = "upload file", accept = ".csv", placeholder = "...", width = "100%", multiple = F)
      ),
      column(
        3,
        actionButton(ns("submit"), label = "submit", width = "100%", style = "margin-right: 0px; font-size:95%;", class = "btn-info")
      ),
      column(
        3,
        downloadButton(ns("download_template"), "Download example template", class = "btn-info", width = "100%", style = "font-size:100%")
      )
    ),

    # IF copmletely WRONG FILE FORMAT LOADED
    conditionalPanel(
      condition = "output.panelCondition_wrongfile",
      ns = NS(id),
      fluidRow(
        column(
          12,
          bslib::card(
            height = 200,
            bslib::card_body(
              p("The files you uploaded doesn't seem to have the right format", style = "text-align:justify;"),
              fluidRow(
                column(4,
                  offset = 4,
                  downloadButton(ns("download_format"), "Download example format", class = "btn-info", width = "100%", style = "font-size:100%")
                )
              )
            )
          )
        )
      )
    ),

    # IF RIGHT FORMAT STILL NEEDS TO MATCH FOR OUR DB

    # CHECKING FILE LOADED - step 1 reassigning columns
    conditionalPanel(
      condition = "output.panelCondition_checkingfile",
      ns = NS(id),
      fluidRow(
        column(
          12,
          bslib::card(
            bslib::card_body(
              fluidRow(
                column(10,
                  offset = 1,
                  DT::DTOutput(ns("cols_table")),
                  div(
                    style = "text-align: right;",
                    actionButton(ns("submit_cols"), label = "next", width = "50%", style = "margin-right: 0px; font-size:95%;", class = "btn-info")
                  ),
                  uiOutput(ns("format_error"))
                )
              )
            )
          )
        )
      )
    ),

    # CHECKING FILE LOADED - step 2 checking the location and time
    conditionalPanel(
      condition = "output.panelCondition_checkingfile_2",
      ns = NS(id),
      fluidRow(
        column(
          12,
          bslib::card(
            bslib::card_body(
              fluidRow(
                column(10,
                  offset = 1,

                  #place
                  h4("About the location of your sites..."),
                  radioButtons(ns("location_type"), label = "What type of location is indicated in your dataset?", choices = c("Lat/y and Lon/x" = "lon_lat", "geometry" = "geom"), selected = "lon_lat", inline = TRUE),
                  radioButtons(ns("location_proj"), label = "What type of projection is your dataset using?", choices = c("WGS84", "BNG"), selected = "WGS84", inline = TRUE),
                  p("If you are not sure, please check the documentation of your dataset or contact the data provider."),

                  conditionalPanel(
                    condition = "input.location_type == 'lon_lat'",
                    ns = NS(id),
                    uiOutput(ns("col_lon_lat"))
                  ),
                  conditionalPanel(
                    condition = "input.location_type == 'geom'",
                    ns = NS(id),
                    uiOutput(ns("col_geom"))
                  ),

                  #time
                  h4("About the time dimension of the dataset..."),
                  radioButtons(ns("time_type"), label = "How is the timing of your dataset indicated?", choices = c("Data format (YY/MM/DD or similar)" = "format_date",
                                                                                                                    "day column, month column and or year column" = "split_date"), selected = "split_date", inline = TRUE),
                  p("At leas one time dimension must be present in your dataset (i.e., the year), others are optional."),

                  conditionalPanel(
                    condition = "input.time_type == 'format_date'",
                    ns = NS(id),
                    uiOutput(ns("col_date_format"))
                  ),

                  conditionalPanel(
                    condition = "input.time_type == 'split_date'",
                    ns = NS(id),
                    uiOutput(ns("col_date_split"))
                  ),

                  #next
                  div(
                    style = "text-align: right;",
                    actionButton(ns("submit_date_and_time"), label = "next", width = "50%", style = "margin-right: 0px; font-size:95%;", class = "btn-info")
                  ),
                  uiOutput(ns("format_error_date_and_time"))
                )
              )
            )
          )
        )
      )
    ),


    # CHECKING FILE LOADED - step 3 asking confirmation
    conditionalPanel(
      condition = "output.panelCondition_rightfile",
      ns = NS(id),
      fluidRow(
        column(
          12,
          bslib::card(
            height = 300,
            bslib::card_body(
              fluidRow(
                column(10,
                  offset = 1,
                  p(lorem::ipsum(10, 4))
                )
              )
            )
          )
        )
      ),
      checkboxInput(ns("acceptCheckbox"), "I accept the terms and conditions"),
      actionButton(ns("submit_to_db"), label = "submit to database", width = "50%", style = "margin-right: 0px; font-size:95%;", class = "btn-info")
    )
    # CLOSE MODULE
  )
}

#' modal_upload Server Functions
#'
#' @noRd
mod_modal_upload_server <- function(id, rv, x) {
  moduleServer(id, session = x, function(input, output, session) {
    ns <- session$ns

    rv_local <- reactiveValues()
    rv_local$file_status <- "none"
    rv_local$loaded_data <- NULL
    rv_local$df <- NULL

    # to show the conditional panel of the columns reassignment
    output$panelCondition_checkingfile <- reactive({
      rv_local$file_status == "checking"
    })
    outputOptions(output, "panelCondition_checkingfile", suspendWhenHidden = FALSE)

    # to show the conditional panel of the time an place reassignment
    output$panelCondition_checkingfile_2 <- reactive({
      rv_local$file_status == "checking_2"
    })
    outputOptions(output, "panelCondition_checkingfile_2", suspendWhenHidden = FALSE)

    #to show the accept and upload
    output$panelCondition_rightfile <- reactive({
      rv_local$file_status == "right"
    })
    outputOptions(output, "panelCondition_rightfile", suspendWhenHidden = FALSE)

    #### uploaded file ###
    observeEvent(input$submit, ignoreInit = T, label = "when uploaded file update rv", {
      rv_local$loaded_data <- readr::read_csv(input$upload$datapath, show_col_types = FALSE)

      # check document fits the general requirements
      if (ncol(rv_local$loaded_data) >= 10) {
        # go to reassign columns values
        rv_local$file_status <- "checking"
      } else {
        # not enough columns to be right go to absolutely wrong format
        rv_local$file_status <- "wrong"
      }
    })

    #### step 1: renaming cols ###
    output$cols_table <- DT::renderDT({
      # the "fixed" table
      df <- data.frame(
        "Name" = c("survey", "site_id", "z", "d", "rho_fe", "f_c", "S_cz"),
        "Description" = c("to fill 1", "unique site code", "to fill 3", "to fill 4", "to fill 5", "to fill 6", "to fill 7"),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      # Build drop down columns using HTML
      names_choices <- c("absent", colnames(rv_local$loaded_data))

      df$column <- vapply(df$Name, function(var_name) {
        soccatoa::render_select(paste0("select_", var_name), names_choices)
      }, character(1))

      df$unit <- vapply(df$Name, function(var_name) {
        soccatoa::render_select(paste0("unit_select_", var_name), soccatoa::get_unit_choices(var_name))
      }, character(1))

      # Save for use elsewhere
      rv_local$df <- df

      # colnames -h
      colnames(df) <- c("Name variable in our database", "What we mean by it", "correspondent column in your data", "unit of your data / projection")

      # display the table with DT
      DT::datatable(
        df,
        escape = FALSE, # need for HTML rendering
        rownames = FALSE,
        selection = "none", # no selection allowe
        options = list(
          dom = "t", # clean look
          ordering = FALSE # no changing of rows order allowed
        )
      )
    })

    observeEvent(input$submit_cols, label = "when submitted the columns save a snapshot of the user selection using JS", {
      dropdown_ids <- c(
        stringr::str_extract(rv_local$df$column, '(?<=id=")[^"]+'), # Extract IDs from the column drop downs
        stringr::str_extract(rv_local$df$unit, '(?<=id=")[^"]+') # Extract IDs from the unit drop downs
      )
      dropdown_ids <- dropdown_ids[!is.na(dropdown_ids)]

      # call the JS function that checks all the selections
      golem::invoke_js("getDropdownValues", dropdown_ids)
    })

    observeEvent(input$dropdown_values, label = "check the values inserted by the user make sense, if so move to the next info; or show error", {
      # check that the columns selected by the user "make sense"
      expected_numeric <- c(
        input$dropdown_values$select_z, input$dropdown_values$select_d, input$dropdown_values$select_rho_fe,
        input$dropdown_values$select_f_c, input$dropdown_values$select_S_cz
      )
      expected_numeric <- expected_numeric[expected_numeric != "absent"]

      expected_character <- c(input$dropdown_values$select_survey, input$dropdown_values$select_site_id)
      expected_character <- expected_numeric[expected_character != "absent"]

      numeric_check <- all(sapply(rv_local$loaded_data[, expected_numeric, drop = FALSE], function(x) {
        suppressWarnings(!any(is.na(as.numeric(as.character(x))) & !is.na(x)))
      }))
      character_check <- all(sapply(rv_local$loaded_data[, expected_character], function(x) {
        all(is.na(x) | is.character(as.character(x)))
      }))

      # check that the columns NEEDE are selcted
      expected_present <- c(input$dropdown_values$select_site_id,
                            input$dropdown_values$select_z, input$dropdown_values$select_d, input$dropdown_values$select_rho_fe,
                            input$dropdown_values$select_f_c, input$dropdown_values$select_S_cz)
      present_check <- all(expected_present != "absent")

      if (numeric_check && character_check && present_check) {
        # adjust to unit (T.B.D.)

        # rename the columns
        for (dropdown_name in names(input$dropdown_values)) {
          if (grepl("unit", dropdown_name)) next # skip unit fields

          old_col <- input$dropdown_values[[dropdown_name]] # e.g. "survey_xyz"
          new_col <- sub("^select_", "", dropdown_name) # e.g. "survey"
          used_old_cols <- NULL
          if (old_col %in% names(rv_local$loaded_data)) {
            # Duplicate the column under new name
            rv_local$loaded_data[[new_col]] <- rv_local$loaded_data[[old_col]]

            # Remember the source column for later removal
            used_old_cols <- c(used_old_cols, old_col)
          }
        }
        # remove old columns
        rv_local$loaded_data <- rv_local$loaded_data[, !(names(rv_local$loaded_data) %in% unique(used_old_cols)), drop = FALSE]

        # do not display an error message
        output$format_error <- renderUI({
          return(NULL)
        }) # no error

        # go to next checking step
        rv_local$file_status <- "checking_2"
      } else {

        #display error text
        error_txt <- c()

        if (!numeric_check) {
          error_txt <- c(error_txt, "Some numeric values are not in the format we expected")
        }

        if (!character_check) {
          error_txt <- c(error_txt, "Some character values are not in the format we expected")
        }

        if(!present_check) {
          error_txt <- c(error_txt, "Some columns that we need are not present in your data")
        }
        output$format_error <- renderUI({
          return(p(paste(error_txt, collapse = " & "), style = "color: red;"))
        })
      }
    })

    #### step 2: check time and place ###
    output$col_lon_lat <- renderUI({
      list(
        fluidRow(column(3,
                        selectInput(ns("col_lon"), "Column with lon/X", choices = c("absent" = "", colnames(rv_local$loaded_data)), selected = "")
                        ),
                 column(3,
                        selectInput(ns("col_lat"), "Column with lat/Y",  choices = c("absent" = "", colnames(rv_local$loaded_data)), selected = "")
                        )
                 )
      )
    })

    output$col_geom <- renderUI({
      selectInput(ns("col_geom"), "Column with geometry", choices = c("absent" = "", colnames(rv_local$loaded_data)), selected = "")
    })

    output$col_date_split <- renderUI({
      list(
        fluidRow(column(3,
                        selectInput(ns("col_day"), "Column with day", choices = c("absent" = "", colnames(rv_local$loaded_data)), selected = "")
                        ),
                 column(3,
                        selectInput(ns("col_month"), "Column with month", choices = c("absent" = "", colnames(rv_local$loaded_data)), selected = "")
                        ),
                 column(3,
                        selectInput(ns("col_year"), "Column with year", choices = c("absent" = "", colnames(rv_local$loaded_data)), selected = "")
                        )
                 )
      )
    })

    output$col_date_format <- renderUI({
      list(
        fluidRow(column(3,
                        selectInput(ns("col_date"), "Column with the date", choices = c("absent" = "",
                                                                                        colnames(rv_local$loaded_data)), selected = "")
                        ),
                 column(3,
                        selectInput(ns("col_date_format"), "Format of the date", choices = c("1969-01-28" = "%Y-%m-%d",
                                                                                             "28/01/1969" = "%d/%m/%Y",
                                                                                             "28/01/69" = "%d/%m/%y",
                                                                                             "01/28/69" = "%m/%d/%Y",
                                                                                             "28-Jan-1969" ="%d-%b-%Y",
                                                                                             "28-Jan-69" = "%d-%b-%y",
                                                                                             "Jan-28-1969" = "%b-%d-%Y"
                                                                                             ), selected = "%d/%m/%Y")
                        )
                 )
      )
    })

    observeEvent(input$submit_date_and_time, label = "check the values inserted by the user make sense, if so move to the next info; or show error", {

      # check that the columns selected by the user "make sense"
      if(input$location_type == "lon_lat") {
        expected_numeric_1 <- c(input$col_lon, input$col_lat)
      } else {
        expected_numeric_1 <-c(input$col_geom)
      }

      if (input$time_type == "split_date") {
        expected_numeric_2 <-  c(input$col_year)
      } else if (input$time_type == "format_date") {
        expected_numeric_2 <-  c(input$col_date)
      }

      expected_numeric <- c(expected_numeric_1, expected_numeric_2) [c(expected_numeric_1, expected_numeric_2) != ""]

      numeric_check <- all(sapply(rv_local$loaded_data[, expected_numeric, drop = FALSE], function(x) {
        suppressWarnings(!any(is.na(as.numeric(as.character(x))) & !is.na(x)))
      }))


      # check that the columns NEEDE are selcted
      if (input$location_type == "lon_lat") {
        expected_present_1 <- c(input$col_lon, input$col_lat)
      } else {
        expected_present_1 <- c(input$col_geom)
      }

      if (input$time_type == "split_date") {
        expected_present_2 <- c(input$col_year)
      } else if (input$time_type == "format_date") {
        expected_present_2 <- c(input$col_date)
      }

      expected_present <- c(expected_present_1, expected_present_2)
      present_check <- all(expected_present != "")

      if (numeric_check && present_check) {

        # adjust dat to match db
        if (input$location_type == "lon_lat") {
          #transform to wgs84
          # rename the columns
          names(rv_local$loaded_data)[names(rv_local$loaded_data) == input$col_lon] <- "lon"
          names(rv_local$loaded_data)[names(rv_local$loaded_data) == input$col_lat] <- "lat"
        } else {
          #set geometry  to wgs84 and split it
        }

        output$format_error_date_and_time <- renderUI({
          return(NULL)
        })
        # go to next checking step
        rv_local$file_status <- "right"

      }else{
        #display error text
        error_txt <- c()

        if (!numeric_check) {
          error_txt <- c(error_txt, "Some numeric values are not in the format we expected")
        }

        if(!present_check) {
          error_txt <- c(error_txt, "Some columns that we need are not present in your data")
        }
        output$format_error_date_and_time <- renderUI({
          return(p(paste(error_txt, collapse = " & "), style = "color: red;"))
        })


      }



    })

    #### step 3: accept and upload to db ###

    #       # check that the columns selected by the user make sense
    #       check <- NULL
    #
    #       #columns that should be numeric
    #
    #
    #       numeric_vector <- tryCatch({ unlist(rv_local$loaded_data[, cols_needed_numeric, drop = FALSE]) }, error = function(e) {c(check, "FALSE")})

    # columns that should be character

    #
    #       #the slected
    #       dropdown_values <- input$dropdown_values
    #       #column name selection
    #       name_choices <- sapply(rv_local$df$Name, function(var) {
    #         input_id <- paste0("select_", var)
    #         dropdown_values[[input_id]] %||% NA_character_
    #       })
    #       #unit selections
    #       unit_choices <- sapply(rv_local$df$`Name varible`, function(var) {
    #         input_id <- paste0("unit_select_", var)
    #         dropdown_values[[input_id]] %||% NA_character_
    #       })
    #
    #       df_selected <- rv_local$df
    #       df_selected$Selected <- name_choices
    #       df_selected$Units <- unit_choices
    #
    #
    #       # get the dataframe with the choices made by the user
    #       n_rows <- nrow(rv_local$df)  # your original df
    #
    #       # Vector to hold dropdown selections
    #       selected_values <- vector("character", length = n_rows)
    #
    #       for (i in seq_len(n_rows)) {
    #         # input IDs must match your dropdowns exactly
    #         dropdown_id <- paste0("select_", i)
    #
    #         # Access input$select_i safely (may be NULL if not selected)
    #         selected_values[i] <- input[[dropdown_id]] %||% NA_character_
    #       }
    #
    #       # Combine with your original df (or however you want)
    #       df_with_selections <- cbind(rv_local$df, Selected = selected_values)
    #
    #       browser()
    #
    #       data_to_save <- data.frame(
    #         "survey" = loaded_data$survey,
    #         "site_id" = loaded_data$site_id,
    #         "year" = loaded_data$year,
    #         "lon" = loaded_data$lon,
    #         "lat" = loaded_data$lat,
    #         "z" = loaded_data$z,
    #         "d" = loaded_data$d,
    #         "rho_fe" = loaded_data$rho_fe,
    #         "f_c" = loaded_data$f_c,
    #         "S_cz" = loaded_data$S_cz,
    #         "user" = rv$user
    #       )
    #
    #       # save the loaded data into the system
    #       load(here::here("data/all_data.rda"))
    #       all_data <- rbind(all_data, data_to_save)
    #
    #       all_data <- all_data %>%
    #         dplyr::distinct(dplyr::across(-user), .keep_all = TRUE)
    #
    #       all_data <- janitor::remove_empty(all_data, which = "rows")
    #
    #       rv$all_data <- all_data
    #
    #       save(all_data, file = here::here("data/all_data.rda"))
    #
    #       # select the site of the uploaded file to run
    #       rv_local$selected_sites <- c(data_to_save$site_id)
    #
    #       rv_local$file_status <- "right"
    #
    #       # close the modal
    #       removeModal()




    observeEvent(rv_local$selected_sites, label = "when db is updated, automatically select the new data", {
      if (!is.null(rv_local$selected_sites)) {
        load(here::here("data/all_data.rda"))
        updateSelectizeInput(session = session, inputId = "sites_picker", selected = sort(rv_local$selected_sites), choices = sort(all_data$site_id))
      }
    })
  })
}

## To be copied in the UI
# mod_modal_upload_ui("modal_upload_1")

## To be copied in the server
# mod_modal_upload_server("modal_upload_1")
