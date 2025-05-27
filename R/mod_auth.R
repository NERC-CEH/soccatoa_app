#' auth UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_auth_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("auth_ui")), # name on tab
  )
}

#' auth Server Functions
#'
#' @noRd
mod_auth_server <- function(id, rv, x) {
  moduleServer(id, session = x, function(input, output, session) {
    ns <- session$ns

    output$auth_ui <- renderUI({
      if (rv$logged_in) {
        tagList(
          span(paste0("my account"), style = "margin-right: 10px;")
        )
      } else {
        tagList(
          span(paste0("login or register"), style = "margin-right: 10px;")
        )
      }
    })
  })
}

## To be copied in the UI
# mod_auth_ui("auth_1")

## To be copied in the server
# mod_auth_server("auth_1")
