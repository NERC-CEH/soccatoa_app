#' tab_explore_db UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_explore_db_ui <- function(id) {
  ns <- NS(id)
  tagList(

    fluidRow(
      column(
        8,
        leaflet::leafletOutput(ns("map_overview"), height = "400px")
      ),
      column(
        4,
        p("Sites available in the database:"),
        uiOutput(ns("select_from_db")),
      fluidRow(
          column(
            6,
            actionButton(ns("select_all"),
                         label = "all",
                         width = "100%",
                         class = "btn-login"
            )
          ),
          column(6,
                 actionButton(ns("select_none"),
                              label = "none",
                              width = "100%",
                              class = "btn-logout"
                 )
          )
        ),

        br (),
        div(actionButton(inputId = ns("explore_data"),
                         label = div(icon("table", lib = "font-awesome"), "Explore the database", width = "30%"),
                         style="margin-left: auto; font-size:100%;", class = "btn-primary"),
            style = "display:flex;"
        )

      )
    )

    #close module
  )
}

#' tab_explore_db Server Functions
#'
#' @noRd
mod_tab_explore_db_server <- function(id, rv, x){
  moduleServer(id, session = x, function(input, output, session){
    ns <- session$ns


    rv_local <- reactiveValues()
    rv_local$sites_selected <- NULL

    #get all the data
    load(here::here("data/database_sites.rda"))

    output$map_overview <- leaflet::renderLeaflet({
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
        leaflet::setView(lng = -1.5, lat = 54.0, zoom = 5)
    })

    output$select_from_db <- renderUI({
      #get the data
      dataset <- soccatoa::database_sites
      # clean in case it's en empty db (first submission)
      database_sites <- janitor::remove_empty(dataset, which = "rows")

      #if there are sites in DB
      if (nrow(database_sites) > 0) {
        return(
          list(
            selectizeInput(
              ns("sites_picker"),
              label = NULL,
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
            label = NULL,
            choices = NULL,
            multiple = FALSE,
            width = "80%"
          ),
          p(
            "The database seems empty at the moment, please upload your dataset",
            style = "text-align:justify;"
          )
        ))
      }
    })

    observeEvent(input$sites_picker, ignoreInit = TRUE, label = "when sites are selected", {

      #nothing selected
      if (is.null(input$sites_picker) || length(input$sites_picker) == 0) {
        rv_local$sites_selected <- NULL
        return()
      } else{
        #something was selected
        rv_local$sites_selected <- dplyr::filter(soccatoa::database_sites,
                                                 site_id %in% input$sites_picker) %>%
          sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
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
            z = paste(unique(round(z, 2)), collapse = ", "),
            d = paste(unique(round(d, 2)), collapse = ", "),
            rho_fe = paste(unique(round(rho_fe, 2)), collapse = ", "),
            f_c = paste(unique(round(f_c, 2)), collapse = ", "),
            S_cz = paste(unique(round(S_cz, 2)), collapse = ", "),
            .groups = "drop"
          )
      }
    })

    observeEvent(rv_local$sites_selected, label = "add/remove markers on map", ignoreNULL = FALSE, {

      #if no sites selected
      if (is.null(rv_local$sites_selected) || nrow(rv_local$sites_selected) == 0) {
        leaflet::leafletProxy(ns("map_overview")) %>%
          leaflet::clearMarkers()
        return()
        #if sites are selected
      }else{
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
          length.out = length(unique(rv_local$sites_selected$site_id))
        )
        site_palette <- leaflet::colorFactor(
          palette = colors,
          domain = rv_local$sites_selected$site_id
        )

        # Map
        #add markers to the map
        leaflet::leafletProxy(ns("map_overview")) %>%
          leaflet::clearMarkers() %>%
          leaflet::addCircleMarkers(
            data = rv_local$sites_selected,
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
      }
    })

    observeEvent(input$select_all, label = "select all", {
      #select all sites
      updateSelectizeInput(session = session, inputId = "sites_picker",
                           selected = sort(unique(soccatoa::database_sites$site_id)))
    })

    observeEvent(input$select_none, label = "select none", {
      #select all sites
      updateSelectizeInput(session = session, inputId = "sites_picker",
                           selected = character(0))
      rv_local$sites_selected <- NULL
    })

    explore_dataset_db <- function() {
      ns <- session$ns
      modalDialog(
        tagList(
          h3("Current Sits in the database"),
          DT::DTOutput(ns("table_db"))
        ),
        size = "xl",
        easyClose = T,
        footer = NULL
      )
    }

    observeEvent(input$explore_data, label = "open modal to explore the dataset", {
      showModal(explore_dataset_db())
    })

    output$table_db <- DT::renderDT({
      #get the data
      dataset <- soccatoa::database_sites
      # clean in case it's en empty db (first submission)
      database_sites <- janitor::remove_empty(dataset, which = "rows")

      if (nrow(database_sites) > 0) {
        DT::datatable(
          database_sites[, (1:10)],
          rownames = FALSE,
          filter = "top",
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            autoWidth = TRUE,
            columnDefs = list(list(className = "dt-center", targets = "_all"))
          )
        )
      } else {
        return()
      }
    })


    #close
  })
}

## To be copied in the UI
# mod_tab_explore_db_ui("tab_explore_db_1")

## To be copied in the server
# mod_tab_explore_db_server("tab_explore_db_1")
