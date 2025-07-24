#' tab_explore_db UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_explore_db_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("test")

  )
}

#' tab_explore_db Server Functions
#'
#' @noRd
mod_tab_explore_db_server <- function(id, rv, x){
  moduleServer(id, session = x, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_tab_explore_db_ui("tab_explore_db_1")

## To be copied in the server
# mod_tab_explore_db_server("tab_explore_db_1")
