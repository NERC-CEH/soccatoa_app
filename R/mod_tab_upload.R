#' tab_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_upload_ui <- function(id) {
  ns <- NS(id)

  tagList(

    #if logged out
    conditionalPanel(
      condition = "output.panelCondition_logged_out",
      ns = NS(id),
      div(
        class = "text-content",
        h4("You must register or login in order to run the model", style = "color: red;"),
      )
    ),

    #if logged in
    conditionalPanel(
      condition = "!output.panelCondition_logged_out",
      ns = NS(id),
      mod_upload_to_DB_ui("upload_to_DB_1")
    )

#close module
  )
}

#' tab_upload Server Functions
#'
#' @noRd
mod_tab_upload_server <- function(id, rv, x){
  moduleServer(id, session = x, function(input, output, session){
    ns <- session$ns

    output$panelCondition_logged_out <- reactive({
      rv$page_showing == "logged_out"
    })
    outputOptions(output, "panelCondition_logged_out", suspendWhenHidden = FALSE)

  })
}

## To be copied in the UI
# mod_tab_upload_ui("tab_upload_1")

## To be copied in the server
# mod_tab_upload_server("tab_upload_1")
