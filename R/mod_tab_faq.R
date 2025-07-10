#' tab_faq UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_faq_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Q1: how does the ....?", style = "text-align: justify;"),
    p(lorem::ipsum(3, 6), style = "text-align: justify;"),
    h3("Q2: why do...?", style = "text-align: justify;"),
    p(lorem::ipsum(3, 8), style = "text-align: justify;"),
    h3("Q3: when ..?", style = "text-align: justify;"),
    p(lorem::ipsum(3, 6), style = "text-align: justify;"),
    h3("Q4: when ..?", style = "text-align: justify;"),
    p(lorem::ipsum(3, 12), style = "text-align: justify;"),
    h3("Q5: when ..?", style = "text-align: justify;"),
    p(lorem::ipsum(3, 3), style = "text-align: justify;"),
    h3("Q6: when ..?", style = "text-align: justify;"),
    p(lorem::ipsum(3, 9), style = "text-align: justify;")
  )
}

#' tab_faq Server Functions
#'
#' @noRd
mod_tab_faq_server <- function(id, rv, x) {
  moduleServer(id, session = x, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_tab_faq_ui("tab_faq_1")

## To be copied in the server
# mod_tab_faq_server("tab_faq_1")
