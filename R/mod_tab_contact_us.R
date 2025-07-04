#' tab_contact_us UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_contact_us_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(lorem::ipsum(2), style = "text-align:justify;"),
    p(
      tagList(shiny::icon("envelope"), "Your e-mail@ceh.ac.uk"),
      style = "font-weight: bold;"
    ),
  )
}

#' tab_contact_us Server Functions
#'
#' @noRd
mod_tab_contact_us_server <- function(id, rv, x) {
  moduleServer(id, session = x, function(input, output, session) {
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_tab_contact_us_ui("tab_contact_us_1")

## To be copied in the server
# mod_tab_contact_us_server("tab_contact_us_1")
