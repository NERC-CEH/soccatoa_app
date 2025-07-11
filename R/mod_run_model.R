#' run_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_run_model_ui <- function(id) {
  ns <- NS(id)
  tagList(

    #run button
    actionButton(
      ns("model_output"),
      label = div(icon("computer", lib = "font-awesome"), "run"),
      width = "100%",
      style = "margin-right: auto; font-size:100%;",
      class = "btn-run"
    )
  )
}

#' run_model Server Functions
#'
#' @noRd
mod_run_model_server <- function(id, rv, x){
  moduleServer(id, session = x, function(input, output, session){
    ns <- session$ns

    run_modal <- function() {
      ns <- session$ns
      modalDialog(
        tagList(
          h2("select years to run"),
          fluidRow(
            column(4, offset = 2, uiOutput(ns("year_start"))),
            column(
              4,
              uiOutput(ns("year_end"))
            )
          ),
          fluidRow(column(
            8,
            offset = 2,
            actionButton(
              inputId = ns("run_model"),
              label = div(icon("computer", lib = "font-awesome"), "run"),
              width = "100%",
              style = "margin-right: auto; font-size:100%;",
              class = "btn-run"
            )
          ))
        ),
        size = "l",
        easyClose = T,
        footer = NULL
      )
    }

    output$year_start <- renderUI({
      selectizeInput(
        ns("year_start_filter"),
        label = "year start",
        width = "100%",
        choices = c(unique(rv$my_data$year)),
        selected = min(unique(rv$my_data$year))
      )
    })

    output$year_end <- renderUI({
      selectizeInput(
        ns("year_end_filter"),
        label = "year end",
        width = "100%",
        choices = c(unique(rv$my_data$year)),
        selected = c(max(rv$my_data$year))
      )
    })

    observeEvent(
      input$model_output,
      label = "show the modal to pick the years",
      {
        showModal(run_modal())
      }
    )

    observeEvent(input$run_model, label = "run the model", {
      years <- c(
        as.numeric(input$year_start_filter),
        as.numeric(input$year_end_filter)
      )

      # run models
      releavant_data <-
        rv$my_data %>%
        dplyr::filter(year >= min(years) & year <= max(years))

      rv$data_results <- soccatoa::run_model_A(df_loaded = releavant_data)

      # rv$data_results_B <- soccatoa::run_model_B(df_loaded = releavant_data,
      # yrstart = as.character(min(years)),
      # yrend = as.character(max(years))
      # )

      # #show the result page
      rv$page_showing <- "results"
      removeModal()
    })


  })
}

## To be copied in the UI
# mod_run_model_ui("run_model_1")

## To be copied in the server
# mod_run_model_server("run_model_1")
