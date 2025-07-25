#' tab_login_register UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tab_login_register_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 10,
        offset = 1,

        # if you are logged out, allow to login
        conditionalPanel(
          condition = "output.panelCondition_logged == false",
          ns = NS(id),
          h3("Login into your account or register a new one"),
          p(lorem::ipsum(2, 5))
        ),

        # if you are logged in, allow to logout
        conditionalPanel(
          condition = "output.panelCondition_logged == true",
          ns = NS(id),
          uiOutput(ns("welcome_user")),
          p(lorem::ipsum(2, 5))
        )
      )
    ),
    fluidRow(
      column(4, offset = 4, uiOutput(ns("login_button")))
    )
  )
}

#' tab_login_register Server Functions
#'
#' @noRd
mod_tab_login_register_server <- function(id, rv, x) {
  moduleServer(id, session = x, function(input, output, session) {
    ns <- session$ns

    rv_local <- reactiveValues()
    rv_local$username_h <- NULL

    output$panelCondition_logged <- reactive({
      rv$logged_in
    })
    outputOptions(output, "panelCondition_logged", suspendWhenHidden = FALSE)

    #### welcome user
    output$welcome_user <- renderUI({
      if (rv$logged_in == TRUE) {
        h3(paste0("Welcome, ", rv_local$username_h, ""))
      }
    })

    #### login or logout button
    output$login_button <- renderUI({
      if (rv$logged_in == TRUE) {
        actionButton(
          ns("login_account"),
          label = "logout",
          # list(icon("door-closed"),"logout"),
          class = "btn-logout",
          width = "100%",
          style = "font-size:110%; padding: 5px; margin-right: auto;"
        )
      } else {
        actionButton(
          ns("login_account"),
          label = "login",
          # list(icon("door-open"),"login"),
          class = "btn-login",
          width = "100%",
          style = "font-size:110%; padding: 5px; margin-right: auto;"
        )
      }
    })

    #### login modal
    login_modal <- function() {
      ns <- session$ns
      modalDialog(
        tagList(
          h2("Login"),
          textInput(
            ns("username"),
            label = "username",
            value = "",
            width = "100%"
          ),
          textInput(
            ns("password"),
            label = "password",
            value = "",
            width = "100%"
          ),
          actionButton(
            ns("login"),
            "login",
            # list(icon("door-open"),"login"),
            class = "btn-login",
            width = "100%",
            style = "font-size:110%; padding: 5px; margin-right: auto;"
          ),
          uiOutput(ns("validating_psw")),
          div(
            style = "display: flex; justify-content: space-between; width: 100%;",
            actionLink(
              ns("register_link"),
              "I am new here!",
              style = "text-decoration: underline;"
            ) # ,
            # p("I forgot my password", style = "text-decoration: underline;")
          )
        ),
        size = "m",
        easyClose = T,
        footer = NULL,
        style = "max-width: 500px; max-height: 90vh; overflow-y: hidden;"
      )
    }

    observeEvent(
      input$login_account,
      ignoreInit = T,
      label = "when login/logout button is clcked",
      {
        # IF YOU ARE LOGGED IN LOGOUT
        if (rv$logged_in == TRUE) {
          rv$logged_in <- FALSE
          rv$page_showing <- "logged_out"

          # back to home
          updateNavbarPage(
            session = x,
            inputId = "main_navbar",
            selected = "SOCCATOA"
          )

          rv$user <- NA
          rv_local$username_h <- NA
          # IF YOU ARE LOGGED OUT LOGIN
        } else {
          showModal(login_modal())
        }
      }
    )

    observeEvent(input$login, label = "login", {
      # check username exists
      load(paste0(here::here(), "/data/database_accounts.rda"))

      all_users <- database_accounts$username

      if (is.null(input$username)) {
        user_given <- c("")
      } else {
        user_given <- c(digest::digest(input$username, algo = "md5"))
      }

      if (user_given %in% all_users) {
        # get the credentials
        if (is.null(input$password)) {
          psw_given <- c("")
        } else {
          psw_given <- c(digest::digest(input$password, algo = "md5"))
        }

        should_be_psw <- dplyr::filter(
          database_accounts,
          username == user_given
        )[1, "password"]

        if (psw_given == should_be_psw) {
          # remove alerts
          output$validating_psw <- renderUI({
            return(NULL)
          })

          # If valid, proceed with logging in
          rv$logged_in <- TRUE

          rv$page_showing <- "logged_in"
          rv$user <- user_given
          rv_local$username_h <- input$username

          # go to page
          updateNavbarPage(
            session = x,
            inputId = "main_navbar",
            selected = "SOCCATOA"
          )

          # Close the modal after successful login
          removeModal()
        } else {
          output$validating_psw <- renderUI({
            p("Wrong username or password", style = "color: #F18968;")
          })
        }
      } else {
        output$validating_psw <- renderUI({
          p("Wrong username or password", style = "color: #F18968;")
        })
      }
    })

    ##### register new user

    register_modal <- function() {
      ns <- session$ns
      modalDialog(
        tagList(
          h2("Create your account"),
          textInput(
            ns("username_register"),
            label = "username",
            value = "",
            width = "100%"
          ),
          textInput(
            ns("password_register"),
            label = "password",
            value = "",
            width = "100%"
          ),
          selectInput(
            ns("use"),
            "main purpose",
            choices = c(
              "Select a use" = "",
              "consultancy",
              "research",
              "other"
            ),
            width = "100%"
          ),
          p("by clicking registering you agree to ..... blah blah blah"),
          actionButton(
            ns("register_user"),
            "register me",
            # list(icon("address-card"),"register me"),
            class = "btn-login",
            width = "100%",
            style = "font-size:110%; padding: 5px; margin-right: auto;"
          ),
          uiOutput(ns("validating_newuser"))
        ),
        size = "m",
        easyClose = T,
        footer = NULL,
        style = "max-width: 500px; max-height: 90vh; overflow-y: hidden;"
      )
    }

    observeEvent(input$register_link, {
      removeModal()
      showModal(register_modal())
    })

    observeEvent(
      input$register_user,
      ignoreInit = T,
      label = "when register account is clicked",
      {
        ## check all the fields are filled
        if (
          !is.null(input$username_register) &&
            !is.null(input$password_register) &&
            input$use != "" &&
            input$username_register != "" &&
            input$password_register != ""
        ) {
          # check username isn't already existing
          load(paste0(here::here(), "/data/database_accounts.rda"))

          if (
            c(digest::digest(input$username_register, algo = "md5")) %in%
              database_accounts$username
          ) {
            output$validating_newuser <- renderUI({
              p("Username already in use", style = "color: #F18968;")
            })
          } else {
            new_user <- data.frame(
              "username" = digest::digest(
                input$username_register,
                algo = "md5"
              ),
              "password" = digest::digest(
                input$password_register,
                algo = "md5"
              ),
              "use" = input$use,
              "date_registered" = Sys.Date()
            )

            database_accounts <- rbind(database_accounts, new_user)

            save(
              database_accounts,
              file = paste0(here::here(), "/data/database_accounts.rda")
            )

            output$validating_newuser <- renderUI({
              p("Account created successfully", style = "color: #58BAC1;")
            })

            rv$logged_in <- TRUE
            rv$page_showing <- "logged_in"

            # go to page
            updateNavbarPage(
              session = x,
              inputId = "main_navbar",
              selected = "SOCCATOA"
            )

            rv$user <- new_user$username
            rv_local$username_h <- input$username_register

            removeModal()
          }
        } else {
          output$validating_newuser <- renderUI({
            p("Must provide all the fields", style = "color: #F18968;")
          })
        }
      }
    )

    # close
  })
}

## To be copied in the UI
# mod_tab_login_register_ui("tab_login_register_1")

## To be copied in the server
# mod_tab_login_register_server("tab_login_register_1")
