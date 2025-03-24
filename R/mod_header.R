#' header UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_header_ui <- function(id) {
  ns <- NS(id)
  tagList(

    fluidRow(
      column(2,
             p(strong("SOCCATOA"), style = "font-size: 40px; color: #0483A4; padding-top: 5px; padding-left: 20px;")
             ),
      column(2,
             offset = 8,
             mod_login_button_ui("login_button_1")
             )

      )


  )
}

#' header Server Functions
#'
#' @noRd
mod_header_server <- function(id, rv, x){
  moduleServer(id, session = x, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_header_ui("header_1")

## To be copied in the server
# mod_header_server("header_1")
