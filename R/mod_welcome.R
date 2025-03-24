#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_welcome_ui <- function(id) {
  ns <- NS(id)
  tagList(

             h3("SOCCATOA"),
             p("Soil carbon markets are emerging to incentivise farmers for management changes that sequester atmospheric carbon.
             To make such schemes verifiable and free from greenwashing, reputable codes need to be established to ensure monitoring is done transparently and with appropriate statistical considerations (Popkin 2023).
             The IEM call is specifically seeking research which provides information on the sequestration ... of carbon in soils in a timely and suitably scaled manner, for both policy audiences and carbon markets and improves on current ...
               methodologies for assessing soil for use in both policy making and carbon markets", style = "text-align: justify;"),
             p(" We propose to create a software tool (a capital asset) for the accreditation of carbon credits claimed on the basis of changes in soil organic carbon (SOC).
             This will capture the joint expertise of UKCEH, BioSS and the University of Glasgow in soil analysis, statistical sampling design, geostatistics, uncertainty quantification, and environmental economics.
             This tool will be designed for any potential regulators or providers of carbon credits, freely accessible via a web app, and will standardise the methods used to assess the magnitude, uncertainty, and potential economic values of SOC change,
             and provide recommendations for the future sampling required.", style = "text-align: justify;")

#close
  )
}

#' welcome Server Functions
#'
#' @noRd
mod_welcome_server <- function(id, rv, x){
  moduleServer(id, session = x, function(input, output, session){
    ns <- session$ns
   #close
  })
}

## To be copied in the UI
# mod_welcome_ui("welcome_1")

## To be copied in the server
# mod_welcome_server("welcome_1")
