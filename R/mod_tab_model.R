#' tab_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_model_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # IF LOGGED OUT
    conditionalPanel(
      condition = "output.panelCondition_logged_out",
      ns = NS(id),
      h4("You must register or login in order to run the model", style = "color: red;"),

    ),

    # IF LOGGED IN
    conditionalPanel(
      condition = "output.panelCondition_logged_in",
      ns = NS(id),
      mod_select_sites_ui("select_sites_1")
    ),

    # IF FILE LOADED
    conditionalPanel(
      condition = "output.panelCondition_rusults",
      ns = NS(id),
      mod_results_ui("results_1")
    )
  )
}

#' tab_model Server Functions
#'
#' @noRd
mod_tab_model_server <- function(id, rv, x) {
  moduleServer(id, session = x, function(input, output, session) {
    ns <- session$ns

    output$panelCondition_logged_out <- reactive({
      rv$page_showing == "logged_out"
    })
    outputOptions(output, "panelCondition_logged_out", suspendWhenHidden = FALSE)

    output$panelCondition_logged_in <- reactive({
      rv$page_showing == "logged_in"
    })
    outputOptions(output, "panelCondition_logged_in", suspendWhenHidden = FALSE)

    output$panelCondition_rusults <- reactive({
      rv$page_showing == "results"
    })
    outputOptions(output, "panelCondition_rusults", suspendWhenHidden = FALSE)

    # close
  })
}

## To be copied in the UI
# mod_tab_model_ui("tab_model_1")

## To be copied in the server
# mod_tab_model_server("tab_model_1")
