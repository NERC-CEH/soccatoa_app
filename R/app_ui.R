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
  UKCEH_theme <- bslib::bs_add_variables(
    UKCEH_theme,
    "headings-font-weight" = 600
  )

  bslib::page_navbar(
    id = "main_navbar",
    theme = UKCEH_theme,

    navbar_options = bslib::navbar_options(
      bg = "#EAEFEC",
      inverse = TRUE
    ),

    # External resources and spinner
    header = tags$head(
      golem_add_external_resources(),
      waiter::use_waiter(),
      waiter::waiter_on_busy(
        waiter::spin_hexdots(),
        color = waiter::transparent(.0)
      )
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
            mod_tab_home_ui("tab_home_1")
          ),
          tabPanel(
            "Upload",
            mod_tab_upload_ui("tab_upload_1")
          ),
          tabPanel(
            "Explore Database",
            mod_tab_explore_db_ui("tab_explore_db_1")
          ),
          tabPanel(
            "Run Model",
            mod_tab_model_ui("tab_model_1")
          )
        )
      ),
      div(
        class = "footer", # defined in custom.css
        mod_footer_ui("footer_1")
      )
    ),
    # FAQ
    bslib::nav_panel(
      title = "FAQs",
      mod_tab_faq_ui("tab_faq_1"),
      div(class = "footer", mod_footer_ui("footer_2"))
    ),
    # CONTACT US
    bslib::nav_panel(
      title = "Contact us",
      mod_tab_contact_us_ui("tab_contact_us_1"),
      div(class = "footer", mod_footer_ui("footer_4"))
    ),
    # TERMS OF USE
    bslib::nav_panel(
      title = "Terms & Privacy",
      mod_tab_terms_privacy_ui("tab_terms_privacy_1"),
      div(class = "footer", mod_footer_ui("footer_3"))
    ),
    # login or logout
    bslib::nav_spacer(),
    bslib::nav_panel(
      title = mod_login_register_title_ui("login_register_title_1"),
      mod_tab_login_register_ui("tab_login_register_1"),
      div(class = "footer", mod_footer_ui("footer_5"))
    ),
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
  golem::add_resource_path(
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
