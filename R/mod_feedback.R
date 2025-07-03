#' feedback UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_feedback_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(lorem::ipsum(2), style = "text-align:justify;"),
    p(
      tagList(shiny::icon("envelope"), "Your e-mail@ceh.ac.uk"),
      style = "font-weight: bold;"
    ),
  )
}

#' feedback Server Functions
#'
#' @noRd
mod_feedback_server <- function(id, rv, x) {
  moduleServer(id, session = x, function(input, output, session) {
    ns <- session$ns

    ### package blastula to send e-mails
  })
}

## To be copied in the UI
# mod_feedback_ui("feedback_1")

## To be copied in the server
# mod_feedback_server("feedback_1")
