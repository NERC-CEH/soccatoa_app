#' terms_privacy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_terms_privacy_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Terms of use & privacy policy"),
    p(lorem::ipsum(14, 4), style = "text-align: justify;")
  )
}

#' terms_privacy Server Functions
#'
#' @noRd
mod_terms_privacy_server <- function(id, rv, x){
  moduleServer( id,  session = x, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_terms_privacy_ui("terms_privacy_1")

## To be copied in the server
# mod_terms_privacy_server("terms_privacy_1")
