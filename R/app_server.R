#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  ###########################
  ### the reactive values ###
  ###########################

  rv <- reactiveValues()

  rv$proj_directory <- "./"
  # rv$proj_directory <- "/data/notebooks/rstudio-madtigsoccatoa/soccatoa/" #when in datalabs

  rv$logged_in <- FALSE
  rv$user <- NA

  rv$page_showing <- "logged_out"

  rv$my_data <- NULL
  rv$data_results <- NULL
  rv$data_results_B <- NULL

  rv$example_locations <- NULL


  rv$all_data <- NULL

  ###########################
  ### the modules ###########
  ###########################
  mod_login_button_server("login_button_1", rv = rv, x = session)

  mod_header_server("header_1", rv = rv, x = session)

  mod_welcome_server("welcome_1", rv = rv, x = session)

  mod_run_model_server("run_model_1", rv = rv, x = session)

  mod_upload_server("upload_1", rv = rv, x = session)
  mod_results_server("results_1", rv = rv, x = session)
  mod_feedback_server("feedback_1", rv = rv, x = session)
  mod_terms_privacy_server("terms_privacy_1", rv = rv, x = session)
  mod_faq_server("faq_1", rv = rv, x = session)

  mod_footer_server("footer_1")
}
