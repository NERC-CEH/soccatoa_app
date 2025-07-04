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
    # IF LOGGED OUT
    conditionalPanel(
      condition = "output.panelCondition_logged_out",
      ns = NS(id),
      h3("SOCCATOA"),
      div(
        img(src = "www/example.jpg", class = "wrapped-image"),
        div(
          class = "text-content",
          p(lorem::ipsum(5, 7))
        )
      )
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

#' run_model Server Functions
#'
#' @noRd
mod_run_model_server <- function(id, rv, x) {
  moduleServer(id, session = x, function(input, output, session) {
    ns <- session$ns

    output$panelCondition_logged_out <- reactive({
      rv$page_showing == "logged_out"
    })
    outputOptions(
      output,
      "panelCondition_logged_out",
      suspendWhenHidden = FALSE
    )

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
# mod_run_model_ui("run_model_1")

## To be copied in the server
# mod_run_model_server("run_model_1")
