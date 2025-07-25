#' Generate HTML select tags for table
#' @description function ...
#' @param id id to use in the select input a character
#' @param choices choices to add as vector
#' @return string that creates select input of id id and choices choices in HTML
#' @export
#'
render_select <- function(id, choices) {
  # if there are elemnts to make a selectinput
  if (is.null(choices) || length(choices) == 0 || all(choices == "")) {
    return("")
  }

  options <- paste0(
    "<option value = ",
    choices,
    ">",
    choices,
    "</option>",
    collapse = ""
  )
  sprintf('<select id="%s" class="form-control">%s</select>', id, options)
}

#' get_unit_choices for each variable
#' @description function to get which units to show for which variables
#' @param var_name id var name to which get units options for
#' @return the choices to display as vector
#' @export
#'
get_unit_choices <- function(var_name) {
  units_choices <- list(
    length = c("m", "cm", "mm"),
    density = c("kg/m³", "g/cm³"),
    carbon_fract = c("fraction", "percent", "g/kg (‰)"),
    carbon = c("kg/m²", "ton/ha", "g/m²")
  )

  var_to_unit_category <- list(
    z = "length",
    d = "length",
    rho_fe = "density",
    f_c = "carbon_fract",
    S_cz = "carbon"
  )

  category <- var_to_unit_category[[var_name]]

  if (!is.null(category) && category %in% names(units_choices)) {
    return(units_choices[[category]])
  }
  return(character(0))
}


#' transform date
#' @description function to transfrom column into right date format
#' @param x column values
#' @param type starting date format
#' @return the column values in date format d/%m/%Y
#' @export
#'
get_right_date_format <- function(x, type) {
  original_dates <- as.Date(x, format = type)

  corrected_dates <- ifelse(
    is.na(original_dates),
    NA,
    format(original_dates, "%d/%m/%Y")
  )

  return(corrected_dates)
}

#' get_standard_unit
#' @description function to transfrom the provided dat into t he DB unit
#' @param my_columns column names in uploaded data
#' @param my_columns_unit columns unit of data uplooaded
#' @param loaded_data data loaded by user
#' @return a df with the variables in the right unit
#' @export
#'

get_standard_unit <- function(my_columns, my_columns_unit, loaded_data) {
  corrected_units <- data.frame()

  for (unit_key in names(my_columns_unit)) {
    # column checked
    column_name <- my_columns[[unit_key]]
    #unit it is
    column_unit <- my_columns_unit[[unit_key]]
    #its values
    values <- loaded_data[[column_name]]

    #Convert to unit used in DB
    new_values <- switch(
      column_unit,

      # Lengths all to meters
      "m" = values,
      "cm" = as.numeric(units::set_units(units::set_units(values, cm), m)),
      "mm" = as.numeric(units::set_units(units::set_units(values, mm), m)),

      # Densities all to kg/m³
      "kg/m³" = values,
      "g/cm³" = as.numeric(units::set_units(
        units::set_units(values, g / cm^3),
        kg / m^3
      )),

      # Fraction conversions
      "fraction" = values,
      "percent" = values / 100,
      "g/kg (‰)" = values / 1000,

      # Mass per area to kg/m²
      "kg/m²" = values,
      "ton/ha" = as.numeric(units::set_units(
        units::set_units(values, t / ha),
        kg / m^2
      )), # 1 ton/ha = 0.1 kg/m²
      "g/m²" = as.numeric(units::set_units(
        units::set_units(values, g / m^2),
        kg / m^2
      )),

      # in case is missing (shouldn't happen)
      values
    )

    loaded_data[[column_name]] <- new_values
  }

  return(loaded_data)
}


#' clean_input
#' @description makes blank input values ("") and NULL and "missing" into NAs
#' @param x an input value
#' @return NA if input is "" or NULL or "missing", otherwise returns x
#' @export
#'
clean_input <- function(x) {
  if (is.null(x) || identical(x, "") || identical(tolower(x), "missing")) {
    NA
  } else {
    x
  }
}

#' Process data for modelling
#'
#' Maybe want to put this elsewhere, but a little bit of data fiddling.
#'
#' What this function does:
#'  - assumes lat/lon columns, EPSG4326
#'  - project to UK grid EPSG27700
#'  - creates new columns called easting and northing
#'  - creates rho_c = f_c*rho_fe
#'  - calculates log rho_c
#'  - makes a factor year variable
#'
#' @param df the data as initially uploaded
#' @return a data.frame that is ready for modelling
#' @importFrom dplyr "%>%"
#' @export
reformat_data <- function(df) {
  # include both northing/easting and lon/lat
  if (!all(c("northing", "easting") %in% names(df))) {
    df <- df %>%
      sf::st_as_sf(coords = c("lon", "lat"), crs = 4236) %>%
      sf::st_transform(27700)

    coords <- df %>%
      sf::st_coordinates()

    df$easting <- coords[, 1]
    df$northing <- coords[, 2]

    df <- df %>%
      sf::st_drop_geometry() # Drop the geometry column
  }
  if (!all(c("lon", "lat") %in% names(df))) {
    df <- df %>%
      sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
      sf::st_transform(4326)
    df <- df %>%
      dplyr::bind_cols(
        sf::st_coordinates(df),
        df[, c("easting", "northing")]
      ) %>%
      dplyr::rename(lon = X, lat = Y) %>% # Rename columns
      sf::st_drop_geometry() # Drop the geometry column
  }

  df %>%
    # get carbon density
    dplyr::mutate(rho_c = f_c * rho_fe) %>%
    dplyr::mutate(log_rho_c = log(rho_c), fyear = as.factor(year))
}
