#' login_register_title UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_login_register_title_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("auth_ui")), # name on tab
  )
}

#' login_register_title Server Functions
#'
#' @noRd
mod_login_register_title_server <- function(id, rv, x) {
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
# mod_login_register_title_ui("login_register_title_1")

## To be copied in the server
# mod_login_register_title_server("login_register_title_1")
