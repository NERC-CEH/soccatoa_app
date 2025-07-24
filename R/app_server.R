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

  rv$logged_in <- FALSE
  rv$user <- NA

  rv$page_showing <- "logged_out"

  rv$my_data <- NULL
  rv$l_results <- NULL
  rv$data_results_B <- NULL

  rv$example_locations <- NULL
  rv$selected_sites_upload <- NULL

  rv$all_data <- NULL

  ###########################
  ### the modules ###########
  ###########################

  #### ordered by tabs ####
  #"Home"
  mod_tab_home_server("tab_home_1", rv = rv, x = session)
  #upload data
  mod_tab_upload_server("tab_upload_1", rv = rv, x = session)
  #explore
  mod_tab_explore_db_server("tab_explore_db_1", rv = rv, x = session)
  #"Run model"
  mod_tab_model_server("tab_model_1", rv = rv, x = session)
  #FAQ
  mod_tab_faq_server("tab_faq_1", rv = rv, x = session)
  #Contact us
  mod_tab_contact_us_server("tab_contact_us_1", rv = rv, x = session)
  #Terms & Privacy
  mod_tab_terms_privacy_server("tab_terms_privacy_1", rv = rv, x = session)
  #Login/register/my account
  mod_tab_login_register_server("tab_login_register_1", rv = rv, x = session)
  mod_login_register_title_server(
    "login_register_title_1",
    rv = rv,
    x = session
  )

  # other modules (nested in the tabs)
  #in tab_model_1
  mod_select_sites_server("select_sites_1", rv = rv, x = session)
  mod_upload_to_DB_server("upload_to_DB_1", rv = rv, x = session)
  mod_run_model_server("run_model_1", rv = rv, x = session)
  mod_results_server("results_1", rv = rv, x = session)
}
