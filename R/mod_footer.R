#' footer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_footer_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(class = "footer-logo-container",
        img( src="https://www.ceh.ac.uk/sites/default/files/images/theme/ukceh_logo_long_720x170_rgb.png", id="ukceh", alt = "UKCEH logo", style="height: 40px;vertical-align:middle; padding-left:15px; padding-right:15px;"),
        img( src="https://upload.wikimedia.org/wikipedia/commons/f/fa/Apple_logo_black.svg", id="apple", alt = "apple logo", style="height: 40px;vertical-align:middle; padding-left:15px; padding-right:15px;"),
        img( src="https://upload.wikimedia.org/wikipedia/commons/4/44/Microsoft_logo.svg", id="microsoft", alt = "microsoft logo", style="height: 40px;vertical-align:middle; padding-left:15px; padding-right:15px;")
    )

  # close
  )
}

#' footer Server Functions
#'
#' @noRd
mod_footer_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_footer_ui("footer_1")

## To be copied in the server
# mod_footer_server("footer_1")
