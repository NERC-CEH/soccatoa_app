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
             h3("Upload a file"),
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
             ),

             h3("Or select sites from the existing database"),
             uiOutput(ns("select_from_db"))

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
                             uiOutput(ns("survey")),
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

      required_cols <- c("survey", "site_id", "lon", "lat", "year", "z", "d", "rho_fe", "f_c", "S_cz")


      if ( all(required_cols  %in% check_cols)){

        #save the loaded data for this run
        rv$my_data <-
          loaded_data %>%
          sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

        #save the loaded data into the system
        load(paste0(rv$proj_directory, "data/all_data.rda"))

        data_to_save <- data.frame("survey" = loaded_data$survey,
                                   "site_id" = loaded_data$site_id,
                                   "year" = loaded_data$year,
                                   "lon" = loaded_data$lon,
                                   "lat" = loaded_data$lat,
                                   "z" = loaded_data$z,
                                   "d" = loaded_data$d,
                                   "rho_fe" = loaded_data$rho_fe,
                                   "f_c" = loaded_data$f_c,
                                   "S_cz" = loaded_data$S_cz,
                                   "user" = rv$user)
        all_data <- rbind(all_data, data_to_save)

        all_data <- all_data %>%
          dplyr::distinct(dplyr::across(-user), .keep_all = TRUE)

        all_data <- janitor::remove_empty(all_data, which = "rows")

        save(all_data, file = paste0(rv$proj_directory, "data/all_data.rda"))

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
        file.copy(paste0(rv$proj_directory, "inst/app/www/downloadables/documentation_facsimile.pdf"), file)
      })


    output$download_format <- downloadHandler(
      filename = paste0("explanation_format_SOCCATOA", ".pdf", sep=""),
      content = function(file) {
        file.copy(paste0(rv$proj_directory, "inst/app/www/downloadables/example_format_soccatoa.pdf"), file)
      })

    output$download_template <- downloadHandler(
      filename = paste0("explanation_format_SOCCATOA", ".pdf", sep=""),
      content = function(file) {
        file.copy(paste0(rv$proj_directory, "inst/app/www/downloadables/example_format_soccatoa.pdf"), file)
      })


    output$survey <- renderUI({
      if(isTruthy(rv$my_data)){
        selectizeInput(ns("survey_picker"), label = "surveys found", choices = unique(rv$my_data$survey), selected = unique(rv$my_data$survey))
      }else{
        return(NULL)
      }

    })


    output$map <- leaflet::renderLeaflet({
      if(isTruthy(rv$my_data) && isTruthy(input$survey_picker)){

        data <-
          rv$my_data%>%
          dplyr::filter(survey %in% input$survey_picker)%>%
          dplyr::select (geometry, loc_id)%>%
          unique()

        leaflet::leaflet() %>%
          htmlwidgets::onRender("function(el, x) {this.zoomControl.setPosition('bottomright');}") %>%
          #the maps background
          leaflet::addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery",options = leaflet::providerTileOptions(zIndex=0, noWrap = TRUE)) %>%
          leaflet::addProviderTiles("OpenStreetMap.Mapnik", options = leaflet::providerTileOptions(zIndex=0, noWrap = TRUE), group = "Streets") %>%
          leaflet::addProviderTiles("Esri.WorldImagery", options = leaflet::providerTileOptions(zIndex=0, noWrap = TRUE), group = "Satellite")%>%
          leaflet::addLayersControl(baseGroups = c("Streets", "Satellite"), options = leaflet::layersControlOptions(collapsed = T, position = "topright")) %>%
          leaflet::addCircleMarkers(data = data,
                                    radius = 4,
                                    color = "#292C2F",
                                    stroke = FALSE,
                                    label = data$loc_id,
                                    fillOpacity = 1)#%>%
          # leaflet::addControl(html = "<h3 style='color: #292C2F; background: transparent; font-size: 18px; font-weight: bold; text-align: center;'>Locations Found</h3>",
          #                     position = "topleft")
      }else{
        return(NULL)
      }
    })

    output$format_description <- renderUI ({
      bslib::card(height = 500,
                  bslib::card_body(
                    p(lorem::ipsum(5,3), style = "text-align:justify;")
                  )
      )
    })

    run_modal <- function(){
      ns <- session$ns
      modalDialog(
        tagList(
          h2("select data to run"),
          p("survey"),
          uiOutput(ns("survey_data")),
          fluidRow(column(6,
                          uiOutput(ns("year_start"))),
                   column(6,
                          uiOutput(ns("year_end")))
          ),
          actionButton(inputId = ns("run_model"), label = div(icon("computer", lib = "font-awesome"),
                                                              "run"),
                       width = "100%", style="margin-right: auto; font-size:100%;", class = "btn-run")
        ),
        size = "m",
        easyClose = T,
        footer = NULL,
        style = "max-width: 500px; max-height: 90vh; overflow-y: hidden;"
      )
    }

    observeEvent(input$model_output, label = "model the results, and go to next page", {

      output$survey_data <- renderUI({
        selectizeInput(ns("survey_filter"),
                       label = "survey to run",
                       choices = c(unique(rv$my_data$survey)),
                       selected = c(unique(rv$my_data$survey)),
                       multiple = TRUE)
      })

      output$year_start <- renderUI({
        if(isTruthy( input$survey_filter)){
          data <- dplyr::filter(rv$my_data,
                                survey %in% input$survey_filter)

          selectizeInput(ns("year_start_filter"),
                         label = "year start",
                         choices = c(unique(data$year)),
                         selected = min(unique(data$year))
                         )
        }else{
          selectizeInput(ns("year_start_filter"),
                         label = "year start",
                         choices = c(unique(rv$my_data$year)),
                         selected = c(min(rv$my_data$year)))
        }
      })

      output$year_end <- renderUI({
        if(isTruthy( input$survey_filter)){
          data <- dplyr::filter(rv$my_data,
                                survey %in% input$survey_filter)

          selectizeInput(ns("year_end_filter"),
                         label = "year end",
                         choices = c(unique(data$year)),
                         selected = max(unique(data$year))
          )
        }else{
          selectizeInput(ns("year_end_filter"),
                         label = "year end",
                         choices = c(unique(rv$my_data$year)),
                         selected = c(max(rv$my_data$year)))
        }
      })

      showModal(run_modal())

    })

    observeEvent(input$run_model, label = "run the model",{

      years <- c(as.numeric(input$year_start_filter),
                 as.numeric(input$year_end_filter))

      #run models
      releavant_data <-
        rv$my_data %>%
        dplyr::filter(survey %in% input$survey_filter) %>%
        dplyr::filter(year >= min(years) & year <= max(years))

      rv$data_results <- soccatoa::run_model_A(df_loaded = releavant_data)

      #rv$data_results_B <- soccatoa::run_model_B(df_loaded = releavant_data,
                                                 #yrstart = as.character(min(years)),
                                                 #yrend = as.character(max(years))
                                                 #)

      # #show the result page
      rv$page_showing <-"results"
      removeModal()
    })


  #close
  })
}



## To be copied in the UI
# mod_upload_ui("upload_1")

## To be copied in the server
# mod_upload_server("upload_1")
