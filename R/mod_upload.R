#' upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_upload_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(

      column(8,
                    fluidRow(column(9,
                                    fileInput(ns("upload"),
                                              label = NULL,
                                              buttonLabel = "upload file",
                                              accept = ".csv",
                                              placeholder = "...",
                                              width = "100%",
                                              multiple = F)
                                     ),
                             column(3,
                                    actionButton(ns("submit"),
                                                 label = "submit",
                                                 width = "100%",
                                                 style = "margin-right: 0px; font-size:95%;",
                                                 class = "btn-info")
                                    )
                             )
                    ),
      column(1,
                    conditionalPanel(
                      condition = "output.panelCondition_rightfile",
                      ns = NS(id),
                      uiOutput(ns("right_format"))
                    ),
                    conditionalPanel(
                      condition = "output.panelCondition_wrongfile",
                      ns = NS(id),
                      uiOutput(ns("wrong_format"))
                    )
                    ),
             column(3,
                    conditionalPanel(
                      condition = "output.panelCondition_rightfile",
                      ns = NS(id),
                      actionButton(inputId = ns("model_output"),
                                   label = div(icon("computer", lib = "font-awesome"),
                                               "run"),
                                   width = "100%", style="margin-right: auto; font-size:100%;", class = "btn-run")

                    )
                    )
             ),

    #IF NO FILE LOADED
    conditionalPanel(
      condition = "output.panelCondition_nofile",
      ns = NS(id),
      p("this is the format that is expected"),
      fluidRow(
        column(4,
               downloadButton(ns("download_template"), "Download example template", class = "btn-info", width = "100%", style = "font-size:100%" )
        )
      )
      ),


    #IF CORRECT FILE LOADED
    conditionalPanel(
      condition = "output.panelCondition_rightfile",
      ns = NS(id),
      fluidRow(
        column(6,
               bslib::card(height = 350,
                           bslib::card_body(
                             class = "p-0",
                             leaflet::leafletOutput(ns("map"))
                             )
                           )
               ),
        column(6,
               bslib::card(height = 350,
                           bslib::card_body(
                             p(lorem::ipsum(1, 2), style = "text-align:justify;"),
                             downloadButton(ns("download_documentation"), "Download documenntation", class = "btn-info", width = "100%", style = "font-size:100%" )

                             )
                           )
               )
        )
      ),

    #IF WRONG FILE LOADED
    conditionalPanel(
      condition = "output.panelCondition_wrongfile",
      ns = NS(id),
      fluidRow(
        column(12,
               bslib::card(height = 200,
                           bslib::card_body(
                             p("The files you uploaded doesn't seem to have the right format", style = "text-align:justify;"),
                             fluidRow(
                               column(4,
                                      offset = 4,
                                      downloadButton(ns("download_format"), "Download example format", class = "btn-info", width = "100%", style = "font-size:100%" )
                                      )
                             )
                             )
                           )
               )
        )
    )


#close
  )
}

#' upload Server Functions
#
#' @noRd
mod_upload_server <- function(id, rv, x){
  moduleServer( id,  session = x, function(input, output, session){
    ns <- session$ns

    rv_local <- reactiveValues()
    rv_local$file_status <- "none"


    output$wrong_format <- renderUI({
      div(class = "icon-container", icon(name = 'remove-sign', lib = "glyphicon"))
    })
    output$right_format <- renderUI({
      div(class = "icon-container", icon(name = 'ok', lib = "glyphicon"))
    })

    observeEvent(input$submit, ignoreInit = T, label = "when uploaded file update rv",{

      #check document fits the requirements
      loaded_data <- readr::read_csv(input$upload$datapath, show_col_types = FALSE)
      check_cols <- colnames(loaded_data)

      required_cols <- c("survey", "site", "site_id", "easting", "northing", "year", "land_use", "land_use_sub", "z")


      if ( all(required_cols  %in% check_cols)){

        #save the loaded data
        rv$my_data <-
          dplyr::filter(loaded_data,
                        survey == unique(loaded_data$survey)[1], # take a survey -- will have to decide what to do
                        year == unique(loaded_data$year)[1], #for now
                        z == 0.55) %>% #for now
          sf::st_as_sf(coords = c("easting", "northing"), crs = 27700)%>%
          sf::st_transform(crs = 4326)

        #show th  cards
        rv_local$file_status <- "right"


      } else {

        rv_local$file_status <- "wrong"

      }
    })


    output$panelCondition_nofile  <- reactive({
      rv_local$file_status == "none"
    })
    outputOptions(output, "panelCondition_nofile", suspendWhenHidden = FALSE)

    output$panelCondition_rightfile  <- reactive({
      rv_local$file_status == "right"
    })
    outputOptions(output, "panelCondition_rightfile", suspendWhenHidden = FALSE)

    output$panelCondition_wrongfile  <- reactive({
      rv_local$file_status == "wrong"
    })
    outputOptions(output, "panelCondition_wrongfile", suspendWhenHidden = FALSE)

    output$download_documentation <- downloadHandler(
      filename = paste0("documentation_SOCCATOA", ".pdf", sep=""),
      content = function(file) {
        file.copy("/data/notebooks/rstudio-madtigsoccatoa/soccatoa/inst/app/www/downloadables/documentation_facsimile.pdf", file)
      })


    output$download_format <- downloadHandler(
      filename = paste0("explanation_format_SOCCATOA", ".pdf", sep=""),
      content = function(file) {
        file.copy("/data/notebooks/rstudio-madtigsoccatoa/soccatoa/inst/app/www/downloadables/example_format_soccatoa.pdf", file)
      })

    output$download_template <- downloadHandler(
      filename = paste0("explanation_format_SOCCATOA", ".pdf", sep=""),
      content = function(file) {
        file.copy("/data/notebooks/rstudio-madtigsoccatoa/soccatoa/inst/app/www/downloadables/example_format_soccatoa.pdf", file)
      })

    output$map <- leaflet::renderLeaflet({
      if(isTruthy(rv$my_data)){
        numPal <- leaflet::colorNumeric('viridis', rv$my_data$S_cz)
        leaflet::leaflet() %>%
          htmlwidgets::onRender("function(el, x) {this.zoomControl.setPosition('bottomright');}") %>%
          #the maps background
          leaflet::addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery",options = leaflet::providerTileOptions(zIndex=0, noWrap = TRUE)) %>%
          leaflet::addProviderTiles("OpenStreetMap.Mapnik", options = leaflet::providerTileOptions(zIndex=0, noWrap = TRUE), group = "Streets") %>%
          leaflet::addProviderTiles("Esri.WorldImagery", options = leaflet::providerTileOptions(zIndex=0, noWrap = TRUE), group = "Satellite")%>%
          leaflet::addLayersControl(baseGroups = c("Streets", "Satellite"), options = leaflet::layersControlOptions(collapsed = T, position = "topright")) %>%
          leaflet::addCircleMarkers(data = rv$my_data,
                                    radius = 4,
                                    color = "#292C2F",
                                    stroke = FALSE,
                                    fillOpacity = 1)%>%
          leaflet::addControl(html = "<h3 style='color: #292C2F; background: transparent; font-size: 18px; font-weight: bold; text-align: center;'>Locations Found</h3>",
                              position = "topleft")
      }
    })

    output$format_description <- renderUI ({
      bslib::card(height = 500,
                  bslib::card_body(
                    p(lorem::ipsum(5,3), style = "text-align:justify;")
                  )
      )
    })


    observeEvent(input$model_output, label = "model the results, and go to next page", {

      #run model
      rv$data_results <- soccatoa::run_model(rv$my_data)

      #show the result page
      rv$page_showing <-"results"
    })


  #close
  })
}



## To be copied in the UI
# mod_upload_ui("upload_1")

## To be copied in the server
# mod_upload_server("upload_1")
