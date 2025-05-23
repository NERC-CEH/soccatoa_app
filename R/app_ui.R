#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  library(magrittr)

  UKCEH_theme <- bslib::bs_theme(
    bg = "#fff",
    fg = "#292C2F",
    primary = "#0483A4",
    secondary = "#EAEFEC",
    success = "#37a635",
    info = "#34b8c7",
    warning = "#F49633",
    base_font = bslib::font_collection("Yummo", "Calibri")
  )
  UKCEH_theme <- bslib::bs_add_variables(UKCEH_theme, "headings-font-weight" = 600)


  ### header
  bslib::page_navbar(
    id = "main_navbar",
    theme = UKCEH_theme,
    bg = "#EAEFEC",
    inverse = TRUE,

    # External resources and spinner
    tags$head(
      golem_add_external_resources(),
      waiter::use_waiter(),
      waiter::waiter_on_busy(waiter::spin_hexdots(), color = waiter::transparent(.0))
    ),

    # MAIN PAGE WITH MODEL
    bslib::nav_panel(
      title = "SOCCATOA",
      div(
        class = "navlist-container",
        navlistPanel(
          widths = c(2, 10),
          id = "soccatoapage",
          tabPanel(
            "Home",
            mod_welcome_ui("welcome_1")
          ),
          tabPanel(
            "Run Model",
            mod_run_model_ui("run_model_1")
          )
        )
      ),
      div(
        class = "footer", # defined in custom.css
        mod_footer_ui("footer_1")
      )
    ),
    # FAQ
    bslib::nav_panel(title = "FAQs", mod_faq_ui("faq_1"),
                     div(class = "footer", mod_footer_ui("footer_2"))),
    # CONTACT US
    bslib::nav_panel(title = "Contact us", mod_feedback_ui("feedback_1"),
                     div(class = "footer", mod_footer_ui("footer_4"))),
    # TERMS OF USE
    bslib::nav_panel(title = "Terms & Privacy", mod_terms_privacy_ui("terms_privacy_1"),
                     div(class = "footer", mod_footer_ui("footer_3"))),
    # login or logout
    bslib::nav_spacer(),
    bslib::nav_panel(title = mod_auth_ui("auth_1"), mod_login_button_ui("login_button_1"),
                     div(class = "footer", mod_footer_ui("footer_5"))),
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "soccatoa"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
