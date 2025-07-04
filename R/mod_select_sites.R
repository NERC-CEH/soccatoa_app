#' upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_select_sites_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        h4("Select sites from the existing database or upload your dataset:")
      ),
      column(
        5,
        uiOutput(ns("select_from_db"))
      ),
      column(
        3,
        br(),
        actionButton(
          ns("select_data"),
          label = "use these sites",
          width = "100%",
          class = "btn-info"
        ),
        uiOutput(ns("warning_no_sites"))
      ),
      column(
        3,
        br(),
        actionButton(
          ns("upload_data"),
          label = div(
            icon("database", lib = "font-awesome"),
            "upload your dataset"
          ),
          width = "100%",
          class = "btn-upload"
        )
      )
    ),

    # IF SITE_ID to run selected
    conditionalPanel(
      condition = "output.panelCondition_rightfile",
      ns = NS(id),
      fluidRow(
        column(
          6,
          bslib::navset_card_tab(
            header = NULL,
            footer = NULL,
            full_screen = TRUE,
            height = 470,
            bslib::nav_panel(
              "Map",
              leaflet::leafletOutput(ns("map"))
            ),
            bslib::nav_panel(
              "Summary",
              DT::DTOutput(ns("table_summary"))
            )
          )
        ),
        column(
          6,
          bslib::card(
            height = 470,
            # class = "p-0",
            bslib::card_body(
              p(lorem::ipsum(1, 2), style = "text-align:justify;"),
              downloadButton(
                ns("download_documentation"),
                "Download documenntation",
                class = "btn-info",
                width = "100%",
                style = "font-size:100%"
              ),
              #run button
              mod_run_model_ui("run_model_1")
            )
          )
        )
      )
    )

    # close
  )
}

#' upload Server Functions
#
#' @noRd
mod_select_sites_server <- function(id, rv, x) {
  moduleServer(id, session = x, function(input, output, session) {
    ns <- session$ns

    rv_local <- reactiveValues()
    rv_local$file_status <- "none"
    rv_local$selected_sites <- NULL
    rv_local$loaded_data <- NULL
    rv_local$df <- NULL

    output$panelCondition_nofile <- reactive({
      rv_local$file_status == "none"
    })
    outputOptions(output, "panelCondition_nofile", suspendWhenHidden = FALSE)

    output$panelCondition_rightfile <- reactive({
      rv_local$file_status == "right"
    })
    outputOptions(output, "panelCondition_rightfile", suspendWhenHidden = FALSE)

    output$panelCondition_wrongfile <- reactive({
      rv_local$file_status == "wrong"
    })
    outputOptions(output, "panelCondition_wrongfile", suspendWhenHidden = FALSE)

    #######################################################
    ############ optional step: uploading your own data ###
    #######################################################

    upload_dataset_modal <- function() {
      ns <- session$ns
      modalDialog(
        tagList(
          mod_upload_to_DB_ui("upload_to_DB_1")
        ),
        size = "xl",
        easyClose = T,
        footer = NULL
      )
    }

    observeEvent(input$upload_data, label = "open upload modal", {
      showModal(upload_dataset_modal())
    })

    observeEvent(
      rv$selected_sites_upload,
      label = "when db is updated, automatically select the new data",
      {
        if (!is.null(rv$selected_sites_upload)) {
          load(here::here("data/database_sites.rda"))
          updateSelectizeInput(
            session = session,
            inputId = "sites_picker",
            selected = sort(rv$selected_sites_upload),
            choices = sort(database_sites$site_id)
          )
        }
      }
    )

    #######################################################
    ############ when sites to run are selected ###########
    #######################################################

    output$select_from_db <- renderUI({
      if (is.null(rv$all_data)) {
        load(here::here("data/database_sites.rda"))

        # clean in case it's en empty db (first submission)
        database_sites <- janitor::remove_empty(database_sites, which = "rows")
      }

      if (nrow(database_sites) > 0) {
        return(
          list(
            selectizeInput(
              ns("sites_picker"),
              label = "Sites available in the database:",
              choices = sort(unique(database_sites$site_id)),
              multiple = TRUE,
              width = "100%"
            )
          )
        )
      } else {
        return(list(
          selectizeInput(
            ns("sites_picker"),
            label = "Sites available in the database:",
            choices = NULL,
            multiple = TRUE,
            width = "80%"
          ),
          p(
            "The database seems empty at the moment, please upload your dataset",
            style = "text-align:justify;"
          )
        ))
      }
    })

    observeEvent(
      input$select_data,
      label = "when selected which sites to run update rv",
      {
        if (is.null(input$sites_picker)) {
          rv_local$file_status <- "none"
          # display why it is not doing anything
          output$warning_no_sites <- renderUI({
            return(p("no sites selected; must select at least one to proceed"))
          })
        } else {
          # remove warning (if it was there)
          output$warning_no_sites <- renderUI({
            return(NULL)
          })

          # load the data
          load(here::here("data/database_sites.rda"))

          rv$my_data <-
            database_sites %>%
            dplyr::filter(site_id %in% input$sites_picker) %>%
            sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

          rv_local$file_status <- "right"
        }
      }
    )

    observeEvent(
      input$sites_picker,
      label = "remove warning and reset if a selection is made",
      {
        output$warning_no_sites <- renderUI({
          return(NULL)
        })
        rv_local$file_status <- "none"
      }
    )

    output$map <- leaflet::renderLeaflet({
      if (isTruthy(rv$my_data)) {
        # clean the data to use on map
        data <-
          rv$my_data %>%
          dplyr::select(
            survey,
            site_id,
            year,
            z,
            d,
            rho_fe,
            f_c,
            S_cz,
            geometry
          ) %>%
          dplyr::group_by(site_id, geometry) %>%
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
          ) # %>%
        # leaflet::addControl(html = "<h3 style='color: #292C2F; background: transparent; font-size: 18px; font-weight: bold; text-align: center;'>Locations Found</h3>",
        #                     position = "topleft")
      } else {
        return(NULL)
      }
    })

    output$format_description <- renderUI({
      bslib::card(
        height = 500,
        bslib::card_body(
          p(lorem::ipsum(5, 3), style = "text-align:justify;")
        )
      )
    })

    output$table_summary <- DT::renderDataTable({
      if (isTruthy(rv$my_data)) {
        data_df <- rv$my_data %>%
          dplyr::bind_cols(sf::st_coordinates(rv$my_data)) %>% # Extract longitude and latitude
          dplyr::rename(lon = X, lat = Y) %>% # Rename columns to 'lon' and 'lat'
          sf::st_drop_geometry() %>% # Drop the geometry column
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
            pageLength = 3,
            searching = TRUE
          )
        )
      } else {
        return(NULL)
      }
    })

    #######################################################
    ############ downloading handles ######################
    #######################################################

    output$download_documentation <- downloadHandler(
      filename = paste0("documentation_SOCCATOA", ".pdf", sep = ""),
      content = function(file) {
        file.copy(
          here::here(
            "inst/app/www/downloadables/documentation_facsimile.pdf"
          ),
          file
        )
      }
    )

    # close
  })
}

## To be copied in the UI
# mod_select_sites_ui("select_sites_1")

## To be copied in the server
# mod_select_sites_server("select_sites_1")
