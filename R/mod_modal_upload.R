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

    # Title
    fluidRow(
      column(
        10,
        offset = 1,
        h5("Upload your dataset in our database")
        )
    ),

    # bar to load the file
    fluidRow(
      column(
        10,
        offset = 1,

        fluidRow(
          column( 6,
                  fileInput(ns("upload"), label = NULL, buttonLabel = "upload file", accept = ".csv", placeholder = "...", width = "100%", multiple = F)
                  ),
          column( 3,
                  actionButton(ns("submit"), label = "submit", width = "100%", style = "margin-right: 0px; font-size:95%;", class = "btn-info")
                  ),
          column( 3,
                 downloadButton(ns("download_template"), "Example template", class = "btn-info", width = "100%")
                 )
          )
        )
    ),

    # IF completely WRONG FILE FORMAT LOADED
    conditionalPanel(
      condition = "output.panelCondition_wrongfile",
      ns = NS(id),
      fluidRow(
        column(
          10,
          offset = 1,

          p("The files you uploaded doesn't seem to have the right format", style = "text-align:justify;"),
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
          10,
          offset = 1,
          DT::DTOutput(ns("cols_table")),
          div(
            style = "text-align: right;",
            actionButton(ns("submit_cols"), label = "next", width = "20%", style = "margin-right: 0px; font-size:95%;", class = "btn-info"),
            uiOutput(ns("format_error"))
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
          10,
          offset = 1,
          fluidRow(column(6,

                          # place
                          h4("Specify the location"),
                          radioButtons(ns("location_proj"), label = "What type of projection is your dataset using?", choices = c("WGS84" = "WGS84",
                                                                                                                                  "OSGrid (BNG, m)" = "BNG"), selected = "WGS84", inline = TRUE),
                          p("If you are not sure, please check the documentation of your dataset or contact the data provider."),
                          uiOutput(ns("col_lon_lat"))

                          ),

                   column(6,

                          #time
                          h4("Specify the time"),
                          radioButtons(ns("time_type"), label = "How is the timing of your dataset indicated?", choices = c(
                            "Data format (YY/MM/DD or similar)" = "format_date",
                            "day column, month column and or year column" = "split_date"
                          ), selected = "split_date", inline = TRUE),

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
                            )
                          )
                   ),

          # next
          div(
            style = "text-align: right;",
            actionButton(ns("submit_date_and_time"), label = "next", width = "20%", style = "margin-right: 0px; font-size:95%;", class = "btn-info"),
            uiOutput(ns("format_error_date_and_time"))
            )
          )
        )
      ),

    # CHECKING FILE LOADED - step 3 see on a map and table
    conditionalPanel(
      condition = "output.panelCondition_rightfile",
      ns = NS(id),

      fluidRow(column(10,
               offset = 1,

               bslib::navset_card_tab(
                 header = NULL,
                 footer = NULL,
                 full_screen = TRUE,
                 height = 450,
                 bslib::nav_panel("Map",
                                  leaflet::leafletOutput(ns("map_preview"))
                                  ),
                 bslib::nav_panel("Summary",
                                  DT::DTOutput(ns("table_preview"))
                                  )

                 ),
               div(
                 style = "text-align: right;",
                 actionButton(ns("map_checked"), label = "next", width = "20%", style = "margin-right: 0px; font-size:95%;", class = "btn-info")
               )
      )
      )
      ),

    # CHECKING FILE LOADED - step 4 asking confirmation
    conditionalPanel(
      condition = "output.panelCondition_submitfile",
      ns = NS(id),

      fluidRow(
        column(
          10,
          offset = 1,
          bslib::card(
            height = 300,
            bslib::card_body(
              h3("Terms & Conditions - understands what happens to your data..."),
              p(lorem::ipsum(10, 4))
              )
            ),
          checkboxInput(ns("acceptCheckbox"), "I accept the terms and conditions"),
          div(
            style = "text-align: right;",
            uiOutput(ns("privacy_text")),
            actionButton(ns("submit_to_db"), label = "submit to database", width = "50%", style = "margin-right: 0px; font-size:95%;", class = "btn-info")
          )
        )
      )

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
    rv_local$to_load <- NULL


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

    # to show the preview map and table
    output$panelCondition_rightfile <- reactive({
      rv_local$file_status == "right"
    })
    outputOptions(output, "panelCondition_rightfile", suspendWhenHidden = FALSE)

    # to show the accept and upload
    output$panelCondition_submitfile <- reactive({
      rv_local$file_status == "submit"
    })
    outputOptions(output, "panelCondition_submitfile", suspendWhenHidden = FALSE)

    #download template
    output$download_template <- downloadHandler(
      filename = paste0("explanation_format_SOCCATOA", ".pdf", sep = ""),
      content = function(file) {
        file.copy(here::here(
          "inst/app/www/downloadables/example_format_soccatoa.pdf"
        ), file)
      }
    )


    #### uploaded file ###
    observeEvent(input$submit, ignoreInit = T, label = "when uploaded file update rv", {
      loaded_data <- readr::read_csv(input$upload$datapath, show_col_types = FALSE)
      #trim all NAS at the bottom (happens sometimes in Excel)
      while (nrow(loaded_data) > 0 && all(is.na(loaded_data[nrow(loaded_data), ]))) {
        loaded_data <- loaded_data[-nrow(loaded_data), ]
      }
      loaded_data <- loaded_data[, colSums(!is.na(loaded_data)) > 0]

      rv_local$loaded_data <- data.frame(loaded_data)

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
      names_choices <- c("missing", colnames(rv_local$loaded_data))

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
      ) %>%

      DT::formatStyle( columns = names(df), backgroundColor = 'transparent', color = 'black' )

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
      my_columns <- list(
        survey = input$dropdown_values$select_survey,
        site_id = input$dropdown_values$select_site_id,
        z = input$dropdown_values$select_z,
        d = input$dropdown_values$select_d,
        rho_fe = input$dropdown_values$select_rho_fe,
        f_c = input$dropdown_values$select_f_c,
        S_cz = input$dropdown_values$select_S_cz
      )

      my_columns_unit <- list(
        z = input$dropdown_values$unit_select_z,
        d = input$dropdown_values$unit_select_d,
        rho_fe = input$dropdown_values$unit_select_rho_fe,
        f_c = input$dropdown_values$unit_select_f_c,
        S_cz = input$dropdown_values$unit_select_S_cz
      )

      # missing to NA
      my_columns <- lapply(my_columns, function(x) {
        if (identical(x, "missing")) NA else x
      })

      # if not NA check they are in the right format
      if (all(!is.na(c(my_columns$z, my_columns$d, my_columns$rho_fe, my_columns$f_c, my_columns$S_cz, my_columns$site_id)))) {
        # check that the columns selected by the user "make sense"
        expected_numeric <- c(my_columns$z, my_columns$d, my_columns$rho_fe, my_columns$f_c, my_columns$S_cz)
        expected_character <- c(my_columns$site_id)

        numeric_check <- all(sapply(rv_local$loaded_data[, expected_numeric, drop = FALSE], function(x) {
          suppressWarnings(!any(is.na(as.numeric(as.character(x))) & !is.na(x)))
        }))
        character_check <- all(sapply(rv_local$loaded_data[, expected_character], function(x) {
          all(is.na(x) | is.character(as.character(x)))
        }))

        # if right format
        if (numeric_check && character_check) {

          # match unit of the DB
          rv_local$loaded_data <- get_standard_unit(my_columns, my_columns_unit, rv_local$loaded_data)
          # # rename the columns
          output_cols <- c("survey", "site_id", "z", "d", "rho_fe", "f_c", "S_cz")

          df_selected <- data.frame(row.names = seq_len(nrow(rv_local$loaded_data)))

          for (colname in output_cols) {
            input_col <- my_columns[[colname]]

            if (is.na(input_col)) {
              # Assign a column of NAs — automatically fills to correct length
              df_selected[[colname]] <- NA
            } else {
              # Assign the appropriate column from the data set
              df_selected[[colname]] <- rv_local$loaded_data[[input_col]]
            }
          }

          rv_local$to_load <- df_selected

          # do not display an error message
          output$format_error <- renderUI({
            return(NULL)
          }) # no error

          # go to next checking step
          rv_local$file_status <- "checking_2"
        } else {
          # display error text
          error_txt <- c()

          if (!numeric_check) {
            error_txt <- c(error_txt, "Some numeric values are not in the format we expected")
          }

          if (!character_check) {
            error_txt <- c(error_txt, "Some character values are not in the format we expected")
          }

          output$format_error <- renderUI({
            return(p(paste(error_txt, collapse = " & "), style = "color: red;"))
          })
        }
      } else {
        error_txt <- c("Some columns that we need are not present in your data")
        output$format_error <- renderUI({
          return(p(paste(error_txt, collapse = " & "), style = "color: red;"))
        })
      }
    })

    #### step 2: check time and place ###
    output$col_lon_lat <- renderUI({
      list(
        fluidRow(
          column(
            5,
            selectInput(ns("col_lon"), "lon/easting", choices = c("missing" = "", colnames(rv_local$loaded_data)), selected = "", width = "100%")
          ),
          column(
            5,
            selectInput(ns("col_lat"), "lat/northing", choices = c("missing" = "", colnames(rv_local$loaded_data)), selected = "", width = "100%")
          )
        )
      )
    })

    output$col_date_split <- renderUI({
      list(
        fluidRow(
          column(
            4,
            selectInput(ns("col_day"), "day", choices = c("missing" = "", colnames(rv_local$loaded_data)), selected = "")
          ),
          column(
            4,
            selectInput(ns("col_month"), "month", choices = c("missing" = "", colnames(rv_local$loaded_data)), selected = "")
          ),
          column(
            4,
            selectInput(ns("col_year"), "year", choices = c("missing" = "", colnames(rv_local$loaded_data)), selected = "")
          )
        )
      )
    })

    output$col_date_format <- renderUI({
      list(
        fluidRow(
          column(
            5,
            selectInput(ns("col_date"), "date", choices = c(
              "missing" = "",
              colnames(rv_local$loaded_data)
            ), selected = "")
          ),
          column(
            5,
            selectInput(ns("col_date_format"), "Format of the date", choices = c(
              "1969-01-28" = "%Y-%m-%d",
              "28/01/1969" = "%d/%m/%Y",
              "28/01/69" = "%d/%m/%y",
              "01/28/69" = "%m/%d/%Y",
              "28-Jan-1969" = "%d-%b-%Y",
              "28-Jan-69" = "%d-%b-%y",
              "Jan-28-1969" = "%b-%d-%Y"
            ), selected = "%d/%m/%Y")
          )
        )
      )
    })

    observeEvent(input$submit_date_and_time, label = "check the values inserted by the user make sense, if so move to the next info; or show error", {

      the_columns <- list(
        lon = input$col_lon,
        lat = input$col_lat,
        year = input$col_year,
        month = input$col_month,
        day = input$col_day,
        date = input$col_date
      )

      # missings to NA
      the_columns <- lapply(the_columns, function(x) {
        if (identical(x, "")) NA else x
      })
      the_columns <- lapply(the_columns, function(x) {
        if (is.null(x)) NA else x
      })

      # check the right one are present
      check_present <- all(!is.na(c(the_columns$lon, the_columns$lat)))

      if (input$time_type == "split_date") {
        check_present <- check_present && !is.na(the_columns$year)
      } else {
        check_present <- check_present && !is.na(the_columns$date)
      }

      # check that the columns selected by the user "make sense"
      if (check_present){

        #numeric
        expected_numeric <- c(the_columns$lon, the_columns$lat)

        if (input$time_type == "split_date") {
          expected_numeric <- c(expected_numeric, the_columns$col_year)
        } else {
          expected_numeric <- c(expected_numeric, the_columns$col_date)
        }

        numeric_check <- all(sapply(rv_local$loaded_data[, expected_numeric, drop = FALSE], function(x) {
          suppressWarnings(!any(is.na(as.numeric(as.character(x))) & !is.na(x)))
        }))

        if(numeric_check){

          ##### proceed to adjust dat to match db

          #############
          ### place ###
          #############

          # transform to wgs84 if not already
          if (input$location_proj == "BNG") {
            # if in BNG transform to wgs84
            coords <- sf::st_as_sf(rv_local$loaded_data, coords = c(the_columns[["lon"]], the_columns[["lat"]]), crs = 27700)
            coords_wgs84 <- sf::st_transform(coords, crs = 4326)
            lons <- sf::st_coordinates(coords_wgs84)[, 1]
            lats <- sf::st_coordinates(coords_wgs84)[, 2]
          }else{
            # if already in wgs84 just split the columns
            lons <- rv_local$loaded_data[[the_columns[["lon"]]]]
            lats <- rv_local$loaded_data[[the_columns[["lat"]]]]
          }

          #check it's in the UK or stop
          if (any(lons < -8 | lons > 2 | lats < 49 | lats > 61)) {
            output$format_error_date_and_time <- renderUI({ return(p("The coordinates you provided are not in the UK, please check your data", style = "color: red;")) })
            return()
          }

          # add to df to load in db
          rv_local$to_load$lon <- lons
          rv_local$to_load$lat <- lats

          #############
          ### time ####
          #############

          if (input$time_type == "split_date"){

            #mandatory: the year
            years <- rv_local$loaded_data[[the_columns[["year"]]]]

            #faculatative: month and day
            if(!is.na(the_columns[["month"]]) ) {
              if ( !any(is.na(as.numeric(as.character(the_columns[["month"]]))) & !is.na(the_columns[["month"]])) ){
                month <- rv_local$loaded_data[[the_columns[["month"]]]]
              } else {
                month <- NA
              }
            }else {
              month <- NA
            }

            if(!is.na(the_columns[["day"]]) ) {
              if ( !any(is.na(as.numeric(as.character(the_columns[["day"]]))) & !is.na(the_columns[["day"]])) ){
                day <- rv_local$loaded_data[[the_columns[["day"]]]]
              } else {
                day <- NA
              }
            } else {
              day   <- NA
            }

          } else {

            ##convert date into right format and extract the year
            dates <- get_right_date_format(rv_local$loaded_data[[the_columns[["date"]] ]], input$col_date_format)
            years  <- as.numeric(substr(dates, nchar(dates) - 3, nchar(dates)))
            month  <-  as.numeric(substr(dates, 4, 5))
            day    <-  as.numeric(substr(dates, 1, 2))
          }

          # add to df to load in db
          rv_local$to_load$year <- years

          #saving month and day if present
          rv_local$to_load$month <- month
          rv_local$to_load$day <- day

          # go to next checking step
          output$format_error_date_and_time <- renderUI({ return(NULL) })

          rv_local$file_status <- "right"

        }else{
          output$format_error_date_and_time <- renderUI({ return(
            p("Some columns that we expected to be numeric are in the wrong format", style = "color: red;"))
          })
        }
      }else{
        output$format_error_date_and_time <- renderUI({ return(
          p("Some data that we were expecting is missing", style = "color: red;"))
        })
      }

    })

    #### step 3: show the map and table ###
    output$map_preview <- leaflet::renderLeaflet({

      if (isTruthy(rv_local$to_load)){

        # clean the data to use on map
        data <-
          rv_local$to_load  %>%
          dplyr::select(survey, site_id, day, month, year, z, d, rho_fe, f_c, S_cz, lon, lat) %>%
          dplyr::group_by(site_id, lon, lat) %>%
          dplyr::summarise(
            year = if (min(year) == max(year)) {
              as.character(min(year))
            } else {
              paste0(min(year), " - ", max(year))
            },
            survey = paste(unique(survey), collapse = ", "),
            z = paste(round(z, 2), collapse = ", "),
            d = paste(round(d, 2), collapse = ", "),
            rho_fe = paste(round(rho_fe, 2), collapse = ", "),
            f_c = paste(round(f_c, 2), collapse = ", "),
            S_cz = paste(round(S_cz, 2), collapse = ", "),
            .groups = "drop"
          )

        # palette
        colors <- rep(c("#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF"),
                      length.out = length(unique(data$site_id))
        )
        site_palette <- leaflet::colorFactor(palette = colors, domain = data$site_id)

        # Map
        leaflet::leaflet() %>%
          htmlwidgets::onRender("function(el, x) {this.zoomControl.setPosition('bottomright');}") %>%
          # the maps background
          leaflet::addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery", options = leaflet::providerTileOptions(zIndex = 0, noWrap = TRUE)) %>%
          leaflet::addProviderTiles("OpenStreetMap.Mapnik", options = leaflet::providerTileOptions(zIndex = 0, noWrap = TRUE), group = "Streets") %>%
          leaflet::addProviderTiles("Esri.WorldImagery", options = leaflet::providerTileOptions(zIndex = 0, noWrap = TRUE), group = "Satellite") %>%
          leaflet::addLayersControl(baseGroups = c("Streets", "Satellite"), options = leaflet::layersControlOptions(collapsed = T, position = "topright")) %>%
          # add the markers of the dataset to run
          leaflet::addCircleMarkers(
            data = data,
            radius = 6,
            color = ~ site_palette(site_id),
            stroke = FALSE,
            popup = ~ paste0(
              "<b>Survey:</b> ", survey, "<br>",
              "<b>Site ID:</b> ", site_id, "<br>",
              "<b>Year:</b> ", year, "<br>",
              "<b>z:</b> ", z, "<br>",
              "<b>d:</b> ", d, "<br>",
              "<b>&rho;_fe:</b> ", rho_fe, "<br>",
              "<b>f_c:</b> ", f_c, "<br>",
              "<b>S_cz:</b> ", S_cz
            ),
            fillOpacity = 1
          )
      } else {
        return(NULL)
      }

    })

    output$table_preview <- DT::renderDataTable({

      if (isTruthy(rv_local$to_load)) {
        data_df <- rv_local$to_load %>%
          dplyr::select(site_id, survey, day, month, year, lon, lat, z, d, rho_fe, f_c, S_cz) %>%
          dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))

        DT::datatable(data_df,
                      options = list(
                        pageLength = 5,
                        searching = TRUE
                      )
        )
      } else {
        return(NULL)
      }
    })

    observeEvent(input$map_checked, label = "mapped was checked", {
      #go on next page
      rv_local$file_status <-  "submit"
    })

    #### step 4: accept and upload to db ###
    observeEvent(input$submit_to_db, label = "check privacy, if so load onto db",{
      if (!input$acceptCheckbox){
        output$privacy_text <- renderUI({
          return(p("You must accept to upload your data", style = "color: red;"))
        })
      }else{
        output$privacy_text <- renderUI({
          return(NULL)
        })

        ###upload to db ##
        data_to_save <- rv_local$to_load
        data_to_save$user <- rv$user

        #save the loaded data into the system
        load(here::here("data/all_data.rda"))
        all_data <- rbind(all_data, data_to_save)

        all_data <- all_data %>%
          dplyr::distinct(dplyr::across(-user), .keep_all = TRUE)

        all_data <- janitor::remove_empty(all_data, which = "rows")
        rv$all_data <- all_data

        save(all_data, file = here::here("data/all_data.rda"))

        # automatically select the site of the uploaded file to run
        rv$selected_sites_upload <- c(data_to_save$site_id)[1]
        rv_local$file_status <- "none"
        # close the modal
        removeModal()
      }

    })

  })
}

## To be copied in the UI
# mod_modal_upload_ui("modal_upload_1")

## To be copied in the server
# mod_modal_upload_server("modal_upload_1")
