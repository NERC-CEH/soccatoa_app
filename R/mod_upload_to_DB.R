#' upload_to_DB UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_upload_to_DB_ui <- function(id) {
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
          column(
            6,
            fileInput(
              ns("upload"),
              label = NULL,
              buttonLabel = "upload file",
              accept = ".csv",
              placeholder = "...",
              width = "100%",
              multiple = F
            )
          ),
          column(
            3,
            actionButton(
              ns("submit"),
              label = "submit",
              width = "100%",
              style = "margin-right: 0px; font-size:95%;",
              class = "btn-info"
            )
          ),
          column(
            3,
            downloadButton(
              ns("download_template"),
              "Example template",
              class = "btn-info",
              width = "100%"
            )
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

          p(
            "The files you uploaded doesn't seem to have the right format",
            style = "text-align:justify;"
          ),
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
            actionButton(
              ns("submit_cols"),
              label = "next",
              width = "20%",
              style = "margin-right: 0px; font-size:95%;",
              class = "btn-info"
            ),
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
          fluidRow(
            column(
              6,

              # place
              h4("Specify the location"),
              radioButtons(
                ns("location_proj"),
                label = "What type of projection is your dataset using?",
                choices = c("WGS84" = "WGS84", "OSGrid (BNG, m)" = "BNG"),
                selected = "WGS84",
                inline = TRUE
              ),
              p(
                "If you are not sure, please check the documentation of your dataset or contact the data provider."
              ),
              uiOutput(ns("col_lon_lat"))
            ),

            column(
              6,

              #time
              h4("Specify the time"),
              radioButtons(
                ns("time_type"),
                label = "How is the timing of your dataset indicated?",
                choices = c(
                  "Data format (YY/MM/DD or similar)" = "format_date",
                  "day column, month column and or year column" = "split_date"
                ),
                selected = "split_date",
                inline = TRUE
              ),

              p(
                "At leas one time dimension must be present in your dataset (i.e., the year), others are optional."
              ),

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
            actionButton(
              ns("submit_date_and_time"),
              label = "next",
              width = "20%",
              style = "margin-right: 0px; font-size:95%;",
              class = "btn-info"
            ),
            uiOutput(ns("format_error_date_and_time"))
          )
        )
      )
    ),

    # CHECKING FILE LOADED - step 3 see on a map and table
    conditionalPanel(
      condition = "output.panelCondition_rightfile",
      ns = NS(id),

      fluidRow(column(
        10,
        offset = 1,

        bslib::navset_card_tab(
          header = NULL,
          footer = NULL,
          full_screen = TRUE,
          height = 450,
          bslib::nav_panel("Map", leaflet::leafletOutput(ns("map_preview"))),
          bslib::nav_panel("Summary", DT::DTOutput(ns("table_preview")))
        ),
        div(
          style = "text-align: right;",
          actionButton(
            ns("map_checked"),
            label = "next",
            width = "20%",
            style = "margin-right: 0px; font-size:95%;",
            class = "btn-info"
          )
        )
      ))
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
              h3(
                "Terms & Conditions - understands what happens to your data..."
              ),
              p(lorem::ipsum(10, 4))
            )
          ),
          checkboxInput(
            ns("acceptCheckbox"),
            "I accept the terms and conditions"
          ),
          div(
            style = "text-align: right;",
            actionButton(
              ns("submit_to_db"),
              label = "submit to database",
              width = "50%",
              style = "margin-right: 0px; font-size:95%;",
              class = "btn-info"
            ),
            uiOutput(ns("error_text"))
          )
        )
      )
    ),

    # CONFIRMING FILE LOADED
    conditionalPanel(
      condition = "output.panelCondition_confirmfile",
      ns = NS(id),
      fluidRow(
        column(
          10,
          offset = 1,
          bslib::card(
            height = 300,
            bslib::card_body(
              h3("Your data has been successfully uploaded!"),
              p("Thank you for contributing to the SOCCATOA database!"),
            )
          )
        )
      ),
      fluidRow(
        column(
          3,
          offset = 5,
          actionButton(
            ns("add_more_data"),
            label = "add more data",
            width = "100%",
            style = "margin-right: 0px; font-size:95%;",
            class = "btn-info"
          )
        ),
        column(
          3,
          actionButton(
            ns("close_modal"),
            label = "close",
            width = "100%",
            style = "margin-right: 0px; font-size:95%;",
            class = "btn-info"
          )
        )
      )
    )
    # CLOSE MODULE
  )
}

#' upload_to_DB Server Functions
#'
#' @noRd
mod_upload_to_DB_server <- function(id, rv, x) {
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
    outputOptions(
      output,
      "panelCondition_checkingfile",
      suspendWhenHidden = FALSE
    )

    # to show the conditional panel of the time an place reassignment
    output$panelCondition_checkingfile_2 <- reactive({
      rv_local$file_status == "checking_2"
    })
    outputOptions(
      output,
      "panelCondition_checkingfile_2",
      suspendWhenHidden = FALSE
    )

    # to show the preview map and table
    output$panelCondition_rightfile <- reactive({
      rv_local$file_status == "right"
    })
    outputOptions(output, "panelCondition_rightfile", suspendWhenHidden = FALSE)

    # to show the accept and upload
    output$panelCondition_submitfile <- reactive({
      rv_local$file_status == "submit"
    })
    outputOptions(
      output,
      "panelCondition_submitfile",
      suspendWhenHidden = FALSE
    )

    # to show the confirmation of upload
    output$panelCondition_confirmfile <- reactive({
      rv_local$file_status == "confirm"
    })
    outputOptions(
      output,
      "panelCondition_confirmfile",
      suspendWhenHidden = FALSE
    )

    #download template
    output$download_template <- downloadHandler(
      filename = paste0("explanation_format_SOCCATOA", ".pdf", sep = ""),
      content = function(file) {
        file.copy(
          here::here(
            "inst/app/www/downloadables/example_format_soccatoa.pdf"
          ),
          file
        )
      }
    )

    #### uploaded file ###
    observeEvent(
      input$submit,
      ignoreInit = T,
      label = "when uploaded file update rv",
      {
        loaded_data <- readr::read_csv(
          input$upload$datapath,
          show_col_types = FALSE
        )
        #trim all NAS at the bottom (happens sometimes in Excel)
        while (
          nrow(loaded_data) > 0 && all(is.na(loaded_data[nrow(loaded_data), ]))
        ) {
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
      }
    )

    #### step 1: renaming cols ###
    output$cols_table <- DT::renderDT({
      # the "fixed" table
      df <- data.frame(
        "Name" = c("survey", "site_id", "z", "d", "rho_fe", "f_c", "S_cz"),
        "Description" = c(
          "to fill 1",
          "unique site code",
          "to fill 3",
          "to fill 4",
          "to fill 5",
          "to fill 6",
          "to fill 7"
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      # Build drop down columns using HTML
      names_choices <- c("missing", colnames(rv_local$loaded_data))

      df$column <- vapply(
        df$Name,
        function(var_name) {
          soccatoa::render_select(paste0("select_", var_name), names_choices)
        },
        character(1)
      )

      df$unit <- vapply(
        df$Name,
        function(var_name) {
          soccatoa::render_select(
            paste0("unit_select_", var_name),
            soccatoa::get_unit_choices(var_name)
          )
        },
        character(1)
      )

      # Save for use elsewhere
      rv_local$df <- df

      # colnames -h
      colnames(df) <- c(
        "Name variable in our database",
        "What we mean by it",
        "correspondent column in your data",
        "unit of your data / projection"
      )

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

        DT::formatStyle(
          columns = names(df),
          backgroundColor = 'transparent',
          color = 'black'
        )
    })

    observeEvent(
      input$submit_cols,
      label = "when submitted the columns save a snapshot of the user selection using JS",
      {
        dropdown_ids <- c(
          stringr::str_extract(rv_local$df$column, '(?<=id=")[^"]+'), # Extract IDs from the column drop downs
          stringr::str_extract(rv_local$df$unit, '(?<=id=")[^"]+') # Extract IDs from the unit drop downs
        )
        dropdown_ids <- dropdown_ids[!is.na(dropdown_ids)]

        # call the JS function that checks all the selections
        golem::invoke_js("getDropdownValues", dropdown_ids)
      }
    )

    observeEvent(
      input$dropdown_values,
      label = "check the values inserted by the user make sense, if so move to the next info; or show error",
      {
        dv <- input$dropdown_values #get the dropdown values

        # organise and convert missing to NA - and treat as such

        my_columns <- lapply(
          list(
            survey = dv$select_survey,
            site_id = dv$select_site_id,
            z = dv$select_z,
            d = dv$select_d,
            rho_fe = dv$select_rho_fe,
            f_c = dv$select_f_c,
            S_cz = dv$select_S_cz
          ),
          clean_input
        )

        my_columns_unit <- list(
          z = dv$unit_select_z,
          d = dv$unit_select_d,
          rho_fe = dv$unit_select_rho_fe,
          f_c = dv$unit_select_f_c,
          S_cz = dv$unit_select_S_cz
        )

        # Ensure all required fields are present
        required_fields <- c("z", "d", "rho_fe", "f_c", "S_cz", "site_id")

        if (!all(!is.na(my_columns[required_fields]))) {
          output$format_error <- renderUI({
            p(
              "Some columns that we need are not present in your data",
              style = "color: red;"
            )
          })
          return()
        }

        # if not NA check they are in the right format: numeric if numeric, charcater if chracter
        df <- rv_local$loaded_data
        numeric_cols <- unlist(my_columns[c("z", "d", "rho_fe", "f_c", "S_cz")])
        char_cols <- unlist(my_columns["site_id"])

        numeric_check <- all(sapply(
          df[, numeric_cols, drop = FALSE],
          function(x) {
            suppressWarnings(
              !any(is.na(as.numeric(as.character(x))) & !is.na(x))
            )
          }
        ))

        character_check <- all(sapply(
          df[, char_cols, drop = FALSE],
          function(x) {
            all(is.na(x) | is.character(as.character(x)))
          }
        ))

        if (numeric_check && character_check) {
          # If format checks pass

          # match unit of the DB
          rv_local$loaded_data <- get_standard_unit(
            my_columns,
            my_columns_unit,
            rv_local$loaded_data
          )

          # Build matched to DB data frame
          rename_mapping <- setNames(names(my_columns), my_columns)
          rv_local$to_load <- rv_local$loaded_data %>%
            dplyr::rename_with(
              ~ rename_mapping[.],
              dplyr::all_of(names(rename_mapping))
            )

          # do not display an error message
          output$format_error <- renderUI({
            return(NULL)
          })

          # go to next checking step
          rv_local$file_status <- "checking_2"
        } else {
          # If format checks did not pass
          error_txt <- c()
          if (!numeric_check) {
            error_txt <- c(
              error_txt,
              "Some numeric values are not correctly formatted"
            )
          }
          if (!character_check) {
            error_txt <- c(
              error_txt,
              "Some character values are not correctly formatted"
            )
          }

          output$format_error <- renderUI({
            p(paste(error_txt, collapse = " & "), style = "color: red;")
          })
        }
      }
    )

    #### step 2: check time and place ###
    output$col_lon_lat <- renderUI({
      list(
        fluidRow(
          column(
            5,
            selectInput(
              ns("col_lon"),
              "lon/easting",
              choices = c("missing" = "", colnames(rv_local$loaded_data)),
              selected = "",
              width = "100%"
            )
          ),
          column(
            5,
            selectInput(
              ns("col_lat"),
              "lat/northing",
              choices = c("missing" = "", colnames(rv_local$loaded_data)),
              selected = "",
              width = "100%"
            )
          )
        )
      )
    })

    output$col_date_split <- renderUI({
      list(
        fluidRow(
          column(
            4,
            selectInput(
              ns("col_day"),
              "day",
              choices = c("missing" = "", colnames(rv_local$loaded_data)),
              selected = ""
            )
          ),
          column(
            4,
            selectInput(
              ns("col_month"),
              "month",
              choices = c("missing" = "", colnames(rv_local$loaded_data)),
              selected = ""
            )
          ),
          column(
            4,
            selectInput(
              ns("col_year"),
              "year",
              choices = c("missing" = "", colnames(rv_local$loaded_data)),
              selected = ""
            )
          )
        )
      )
    })

    output$col_date_format <- renderUI({
      list(
        fluidRow(
          column(
            5,
            selectInput(
              ns("col_date"),
              "date",
              choices = c(
                "missing" = "",
                colnames(rv_local$loaded_data)
              ),
              selected = ""
            )
          ),
          column(
            5,
            selectInput(
              ns("col_date_format"),
              "Format of the date",
              choices = c(
                "1969-01-28" = "%Y-%m-%d",
                "28/01/1969" = "%d/%m/%Y",
                "28/01/69" = "%d/%m/%y",
                "01/28/69" = "%m/%d/%Y",
                "28-Jan-1969" = "%d-%b-%Y",
                "28-Jan-69" = "%d-%b-%y",
                "Jan-28-1969" = "%b-%d-%Y"
              ),
              selected = "%d/%m/%Y"
            )
          )
        )
      )
    })

    observeEvent(
      input$submit_date_and_time,
      label = "check the values inserted by the user make sense, if so move to the next info; or show error",
      {
        # organise the input values and set to NA the missing ones
        the_columns <- lapply(
          list(
            lon = input$col_lon,
            lat = input$col_lat,
            year = input$col_year,
            month = input$col_month,
            day = input$col_day,
            date = input$col_date
          ),
          clean_input
        )

        # check the right one are present
        check_present <- all(!is.na(c(the_columns$lon, the_columns$lat)))

        if (input$time_type == "split_date") {
          check_present <- check_present && !is.na(the_columns$year)
        } else {
          check_present <- check_present && !is.na(the_columns$date)
        }

        #stop if some columns are missing
        if (!check_present) {
          output$format_error_date_and_time <- renderUI({
            p(
              "Some data that we were expecting is missing",
              style = "color: red;"
            )
          })
          return()
        }

        # if all present check they are in the right format
        df <- rv_local$to_load
        numeric_cols <- c(the_columns$lon, the_columns$lat)

        if (input$time_type == "split_date") {
          numeric_cols <- c(numeric_cols, the_columns$year)
        }

        numeric_check <- all(sapply(
          df[, numeric_cols, drop = FALSE],
          function(x) {
            suppressWarnings(
              !any(is.na(as.numeric(as.character(x))) & !is.na(x))
            )
          }
        ))

        #if not in the right format stop
        if (!numeric_check) {
          output$format_error_date_and_time <- renderUI({
            p(
              "Some columns that we expected to be numeric are in the wrong format",
              style = "color: red;"
            )
          })
          return()
        }

        # proceed to adjust place and time  to match db
        ### place ####
        # check / delete missing rows warning user
        lons <- df[[the_columns$lon]]
        lats <- df[[the_columns$lat]]
        missing_location <- is.na(lons) | is.na(lats)
        num_missing_loc <- sum(missing_location)

        if (num_missing_loc == nrow(df)) {
          showNotification(
            "All rows have missing location data. Cannot proceed.",
            type = "error", # 'message', 'warning', 'error'
            duration = NULL
          )
          return()
        }
        if (num_missing_loc > 0) {
          showNotification(
            "Some rows have missing location data. These will be excluded.",
            type = "warning",
            duration = NULL
          )
          df <- df[!missing_location, ]
        }

        # transform to wgs84 if not already
        if (input$location_proj == "BNG") {
          # if in BNG transform to wgs84
          coords <- sf::st_as_sf(
            df,
            coords = c(the_columns[["lon"]], the_columns[["lat"]]),
            crs = 27700
          )
          coords_wgs84 <- sf::st_transform(coords, crs = 4326)
          df$lons <- sf::st_coordinates(coords_wgs84)[, 1]
          df$lats <- sf::st_coordinates(coords_wgs84)[, 2]
        } else {
          # if already in wgs84 just split the columns
          df$lons <- df[[the_columns$lon]]
          df$lats <- df[[the_columns$lat]]
        }

        # check / delete rows in UK plus warning user
        outside_uk <- which(
          df$lons < -8 | df$lons > 2 | df$lats < 49 | df$lats > 61
        )

        if (length(outside_uk) == nrow(df)) {
          showNotification(
            "All rows have data outside the UK. Cannot proceed.",
            type = "error",
            duration = NULL
          )
          return()
        }
        if (length(outside_uk) > 0) {
          showNotification(
            "Some rows have data outside the UK. These will be excluded.",
            type = "warning",
            duration = NULL
          )
          df <- df[-c(outside_uk), ] #remove the outside values
        }

        ### time ####
        if (input$time_type == "split_date") {
          df$years <- df[[the_columns$year]]
          df$month <- if (!is.na(the_columns$month)) {
            df[[the_columns$month]]
          } else {
            NA
          }
          df$day <- if (!is.na(the_columns$day)) df[[the_columns$day]] else NA
        } else {
          #convert to right format to split
          dates <- get_right_date_format(
            df[[the_columns$date]],
            input$col_date_format
          )

          df$years <- as.numeric(substr(dates, nchar(dates) - 3, nchar(dates)))
          df$month <- as.numeric(substr(dates, 4, 5))
          df$day <- as.numeric(substr(dates, 1, 2))
        }

        #if missing dates delete rows and warn user
        invalid_years <- which(is.na(df$year))

        if (length(invalid_years) == nrow(df)) {
          showNotification(
            "All rows have missing year data. Cannot proceed.",
            type = "error",
            duration = NULL
          )
          return()
        }
        if (length(invalid_years) > 0) {
          showNotification(
            "Some rows have missing year data. These will be excluded.",
            type = "warning",
            duration = NULL
          )
          df <- df[-c(invalid_years), ] #remove the missin years values
        }

        rv_local$to_load <- df %>%
          dplyr::select(
            survey,
            site_id,
            z,
            d,
            rho_fe,
            f_c,
            S_cz,
            lon = lons,
            lat = lats,
            year = years,
            month,
            day
          )

        #go to next
        output$format_error_date_and_time <- renderUI(NULL)
        rv_local$file_status <- "right"
      }
    )

    #### step 3: show the map and table ###
    output$map_preview <- leaflet::renderLeaflet({
      if (isTruthy(rv_local$to_load)) {
        # clean the data to use on map
        data <-
          rv_local$to_load %>%
          dplyr::select(
            survey,
            site_id,
            day,
            month,
            year,
            z,
            d,
            rho_fe,
            f_c,
            S_cz,
            lon,
            lat
          ) %>%
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
        colors <- rep(
          c(
            "#FF7F0E",
            "#2CA02C",
            "#D62728",
            "#9467BD",
            "#8C564B",
            "#E377C2",
            "#7F7F7F",
            "#BCBD22",
            "#17BECF"
          ),
          length.out = length(unique(data$site_id))
        )
        site_palette <- leaflet::colorFactor(
          palette = colors,
          domain = data$site_id
        )

        # Map
        leaflet::leaflet() %>%
          htmlwidgets::onRender(
            "function(el, x) {this.zoomControl.setPosition('bottomright');}"
          ) %>%
          # the maps background
          leaflet::addProviderTiles(
            "Esri.WorldImagery",
            group = "Esri.WorldImagery",
            options = leaflet::providerTileOptions(zIndex = 0, noWrap = TRUE)
          ) %>%
          leaflet::addProviderTiles(
            "OpenStreetMap.Mapnik",
            options = leaflet::providerTileOptions(zIndex = 0, noWrap = TRUE),
            group = "Streets"
          ) %>%
          leaflet::addProviderTiles(
            "Esri.WorldImagery",
            options = leaflet::providerTileOptions(zIndex = 0, noWrap = TRUE),
            group = "Satellite"
          ) %>%
          leaflet::addLayersControl(
            baseGroups = c("Streets", "Satellite"),
            options = leaflet::layersControlOptions(
              collapsed = T,
              position = "topright"
            )
          ) %>%
          # add the markers of the dataset to run
          leaflet::addCircleMarkers(
            data = data,
            radius = 6,
            color = ~ site_palette(site_id),
            stroke = FALSE,
            popup = ~ paste0(
              "<b>Survey:</b> ",
              survey,
              "<br>",
              "<b>Site ID:</b> ",
              site_id,
              "<br>",
              "<b>Year:</b> ",
              year,
              "<br>",
              "<b>z:</b> ",
              z,
              "<br>",
              "<b>d:</b> ",
              d,
              "<br>",
              "<b>&rho;_fe:</b> ",
              rho_fe,
              "<br>",
              "<b>f_c:</b> ",
              f_c,
              "<br>",
              "<b>S_cz:</b> ",
              S_cz
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
          dplyr::select(
            site_id,
            survey,
            day,
            month,
            year,
            lon,
            lat,
            z,
            d,
            rho_fe,
            f_c,
            S_cz
          ) %>%
          dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))

        DT::datatable(
          data_df,
          options = list(
            pageLength = 5,
            searching = TRUE
          ),
          selection = "none"
        )
      } else {
        return(NULL)
      }
    })

    observeEvent(input$map_checked, label = "mapped was checked", {
      #go on next page
      rv_local$file_status <- "submit"
    })

    #### step 4: accept and upload to db ###
    observeEvent(
      input$submit_to_db,
      label = "check privacy, if so load onto db",
      {
        if (!input$acceptCheckbox) {
          output$error_text <- renderUI({
            p("You must accept to upload your data", style = "color: red;")
          })
          return()
        }

        #clean error
        output$error_text <- renderUI(NULL)

        #upload to db
        data_to_save <- rv_local$to_load
        data_to_save$user <- rv$user

        #save the loaded data into the system
        load(here::here("data/database_sites.rda"))

        # Check for duplicates (ignoring 'user' column)
        temp_all <- dplyr::select(database_sites, -user)
        temp_new <- dplyr::select(data_to_save, -user)

        duplicated_rows <- dplyr::semi_join(
          temp_new,
          temp_all,
          by = colnames(temp_new)
        )
        num_duplicates <- nrow(duplicated_rows)
        num_new <- nrow(data_to_save) - num_duplicates

        if (num_duplicates == nrow(data_to_save)) {
          # All rows are duplicates — show modal and don't save
          showNotification(
            "All duplicates detected. This data is already in the database",
            type = "error",
            duration = NULL
          )
          return() #stop here add nothing
        }

        if (num_duplicates > 0 && num_new > 0) {
          # Some duplicates — show modal, but add new data
          showNotification(
            "Some duplicates detected. These will be excluded.",
            type = "warning",
            duration = NULL
          )
        }

        # Filter out duplicates before saving
        data_to_save_unique <- dplyr::anti_join(
          data_to_save,
          duplicated_rows,
          by = colnames(temp_new)
        )

        #save
        database_sites <- rbind(database_sites, data_to_save_unique)
        database_sites <- janitor::remove_empty(database_sites, which = "rows")
        rv$all_data <- database_sites

        save(database_sites, file = here::here("data/database_sites.rda"))

        # automatically select the site of the uploaded file to run
        rv$selected_sites_upload <- data_to_save_unique$site_id[1]

        #go on next page
        rv_local$file_status <- "confirm"
      }
    )

    #### step 5:  confirm ###
    observeEvent(input$add_more_data, label = "add more data", {
      # go back to the first page
      rv_local$file_status <- "none"
    })

    observeEvent(input$close_modal, label = "close modal", {
      # close the modal
      removeModal()
    })
  })
}

## To be copied in the UI
# mod_upload_to_DB_ui("upload_to_DB_1")

## To be copied in the server
# mod_upload_to_DB_server("upload_to_DB_1")
