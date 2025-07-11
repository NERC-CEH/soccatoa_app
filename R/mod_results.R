#' results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    ##### back and downloads
    div(
      style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
      # Back button, aligned to the right
      div(
        style = "flex: 1; text-align: left;",
        actionButton(
          inputId = ns("back_upload"),
          label = div(
            icon("chevron-left", lib = "font-awesome"),
            "back",
            width = "30%"
          ),
          style = "font-size:100%;",
          class = "btn-danger"
        )
      ),
      # Other three download buttons aligned to the left
      div(
        style = "flex: 3; display: flex; justify-content: flex-end; gap: 2px;",
        downloadButton(
          ns("download_documentation"),
          "Documentation",
          class = "btn-info",
          style = "font-size:100%; display: inline-block;"
        ),
        downloadButton(
          ns("download_report"),
          "Report",
          class = "btn-danger",
          style = "font-size:100%; display: inline-block;"
        ),
        downloadButton(
          ns("download_rawdata"),
          "Raw Data",
          class = "btn-danger",
          style = "font-size:100%; display: inline-block;"
        )
      )
    ),

    ############ results

    div(
      style = "margin-top: 15px;",
      fluidRow(
        column(
          6,
          bslib::navset_card_underline(
            height = 500,
            bslib::nav_panel(
              "Graph",
              plotOutput(ns("result"))
            ),
            bslib::nav_panel(
              "Guide",
              shiny::img(
                src = "www/example_guide.png",
                align = "center",
                style = "max-width: 100%;"
              )
            )
          )
        ),
        column(
          6,
          bslib::navset_card_underline(
            height = 500,
            bslib::nav_panel(
              "Uncertainty Graph",
              plotOutput(ns("uncertainty_graph"))
            ),
            bslib::nav_panel(
              "Map",
              id = "map",
              leaflet::leafletOutput(ns("map_result"), height = "100%")
            )
          )
        )
      )
    )

    # close
  )
}

#' results Server Functions
#'
#' @noRd
mod_results_server <- function(id, rv, x) {
  moduleServer(id, session = x, function(input, output, session) {
    ns <- session$ns

    output$result <- renderPlot({
      if (isTruthy(rv$data_results)) {
        # bind stats model and climate model results
        data_2 <- cbind(
          summarize_results_change(rv$data_results),
          # this is just the example data
          data.frame(
            climate_effect = c(2, 3), # blue line (constant)
            climate_error = c(0.3, 0.4) # Error for blue
          )
        )

        # Convert data to long format for easier legend handling
        data_long <- tidyr::pivot_longer(
          data_2,
          cols = c(total, climate_effect),
          names_to = "category",
          values_to = "value"
        )

        data_2$color_ribbon <- "striped area"

        # Plot
        ggplot2::ggplot(
          data_long,
          ggplot2::aes(x = time, y = value, color = category, group = category)
        ) +
          # Shaded area
          ggplot2::geom_ribbon(
            data = data_2,
            ggplot2::aes(
              x = time,
              ymin = climate_effect,
              ymax = total,
              fill = color_ribbon
            ),
            alpha = 0.5,
            inherit.aes = FALSE
          ) +

          # Climate effect line
          ggplot2::geom_line(size = 1.5) +

          # Error bars
          ggplot2::geom_errorbar(
            data = data_2,
            ggplot2::aes(
              x = time,
              ymin = total - total_error,
              ymax = total + total_error,
              color = "total"
            ),
            width = 0.2,
            inherit.aes = FALSE
          ) +
          ggplot2::geom_errorbar(
            data = data_2,
            ggplot2::aes(
              x = time,
              ymin = climate_effect - climate_error,
              ymax = climate_effect + climate_error,
              color = "climate effect"
            ),
            width = 0.2,
            inherit.aes = FALSE
          ) +
          # Axis labels
          ggplot2::labs(x = NULL, y = NULL, color = "Legend", fill = "Legend") + # Remove global x label
          ggplot2::scale_color_manual(
            values = c("total" = "#0483A4", "climate effect" = "#F49633")
          ) +
          ggplot2::scale_fill_manual(
            name = "Result",
            values = c("striped area" = "#37a635")
          ) + # Updated fill mapping

          ggplot2::theme(
            legend.background = ggplot2::element_blank(),
            legend.box.background = ggplot2::element_blank(),
            legend.key = ggplot2::element_rect(
              fill = "transparent",
              color = NA
            ),
            legend.text = ggplot2::element_text(size = 14),
            panel.background = ggplot2::element_blank(),
            panel.border = ggplot2::element_rect(
              color = "#EAEFEC",
              fill = NA,
              size = 1
            ),
            panel.grid.major = ggplot2::element_line(color = "#EAEFEC"),
            plot.title = ggplot2::element_text(
              size = 20,
              face = "bold",
              hjust = 0.5
            ),
            axis.title.x = ggplot2::element_text(size = 16),
            axis.title.y = ggplot2::element_text(size = 16),
            axis.text = ggplot2::element_text(size = 14)
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            axis.title.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(size = 12) # Ensure x-axis labels are visible
          )
      } else {
        return(NULL)
      }
    })

    output$uncertainty_graph <- renderPlot({
      if (isTruthy(rv$data_results)) {
        # get distribution of differences
        uncertainty_df <- summarize_results_dist(rv$data_results)

        ggplot2::ggplot(uncertainty_df) +
          ggplot2::geom_density(
            ggplot2::aes(value),
            color = "#37a635",
            size = 1.5
          ) +
          ggplot2::labs(
            title = "Distribution of difference\nin soil carbon",
            x = "Difference",
            y = "Probability density"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(
              size = 20,
              face = "bold",
              hjust = 0.5
            ),
            axis.title.x = ggplot2::element_text(size = 16),
            axis.title.y = ggplot2::element_text(size = 16),
            axis.text = ggplot2::element_text(size = 14),
            panel.border = ggplot2::element_rect(
              color = "#EAEFEC",
              fill = NA,
              size = 1
            )
          )
      } else {
        return(NULL)
      }
    })

    output$map_result <- leaflet::renderLeaflet({
      if (isTruthy(rv$data_results)) {
        data <- summarize_results_simple(rv$data_results) %>%
          dplyr::filter(z == 0.55) %>%
          sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
          sf::st_transform(crs = 4326) %>%
          # calculate S_cz at the depth we've selected
          dplyr::mutate(
            S_cz = d * rho_c
          )

        numPal <- leaflet::colorNumeric("viridis", data$S_cz)
        leaflet::leaflet() %>%
          # background
          leaflet::addProviderTiles(
            "Esri.WorldImagery",
            group = "Esri.WorldImagery",
            options = leaflet::providerTileOptions(zIndex = 0, noWrap = TRUE)
          ) %>%
          leaflet::addProviderTiles(
            "OpenStreetMap.Mapnik",
            options = leaflet::providerTileOptions(zIndex = 0, noWrap = TRUE),
            group = "Streets"
          ) %>%
          leaflet::addProviderTiles(
            "Esri.WorldImagery",
            options = leaflet::providerTileOptions(zIndex = 0, noWrap = TRUE),
            group = "Satellite"
          ) %>%
          leaflet::addLayersControl(
            baseGroups = c("Streets", "Satellite"),
            options = leaflet::layersControlOptions(
              collapsed = T,
              position = "topright"
            )
          ) %>%
          # draw the dots with size based on
          leaflegend::addSymbolsSize(
            data = data,
            values = ~S_cz, # size
            shape = "circle",
            color = "black",
            fillColor = ~ numPal(S_cz), # color
            opacity = .5,
            baseSize = 10
          ) %>%
          leaflegend::addLegendSize(
            values = data$S_cz,
            color = "black",
            title = "S_cz_2004",
            shape = "circle",
            orientation = "horizontal",
            opacity = .5,
            fillOpacity = 0,
            breaks = 5,
            position = "bottomright"
          ) %>%
          leaflegend::addLegendNumeric(
            pal = numPal,
            title = "S_cz_2004",
            shape = "stadium",
            values = data$S_cz,
            fillOpacity = .5,
            decreasing = TRUE,
            position = "bottomright"
          )
      } else {
        return(NULL)
      }
    })

    output$download_documentation <- downloadHandler(
      filename = paste0("documentation_SOCCATOA", ".pdf", sep = ""),
      content = function(file) {
        file.copy(
          paste0(
            here::here(),
            "/inst/app/www/downloadables/documentation_facsimile.pdf"
          ),
          file
        )
      }
    )

    output$download_rawdata <- downloadHandler(
      filename = paste0("results_SOCCATOA", ".csv", sep = ""),
      content = function(file) {
        write.csv(rv$my_data, file)
      }
    )

    output$download_report <- downloadHandler(
      filename = function() {
        paste0(
          "soccatoa_report",
          "_",
          stringr::str_replace_all(Sys.Date(), "-", "_"),
          ".html"
        )
        # paste0("soccatoa_report", "_", stringr::str_replace_all(Sys.Date(), "-", "_"), ".pdf")
      },
      content = function(file) {
        tempReport <- paste0(
          here::here(),
          "/inst/app/www/downloadables/example_report.Rmd"
        )

        params_list <- list(
          my_data = rv$my_data # Make sure this is correct for your reactive value
        )

        # Render the Rmd file with the provided parameters
        rmarkdown::render(
          input = tempReport, # Path to the Rmd file
          output_file = file, # Output file to be downloaded
          output_format = "html_document", # Set the output format
          params = params_list, # Pass the parameters (like my_data)
          envir = new.env() # Use a new environment for rendering
        )
      }
    )

    observeEvent(input$back_upload, label = "go back to upload page", {
      rv$page_showing <- "logged_in"
    })

    # close
  })
}

## To be copied in the UI
# mod_results_ui("results_1")

## To be copied in the server
# mod_results_server("results_1")
