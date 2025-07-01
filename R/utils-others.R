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

  options <- paste0("<option value = ", choices, ">", choices, "</option>", collapse = "")
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
    length = c("cm", "m", "mm", "yards", "inches"),
    density = c("g/cm3", "mg/m3", "g/m3"),
    carbon = c("t/ha", "mg/ha", "Kg/m2")
  )

  unit_map <- list(
    length = c("z", "d"),
    density = c("rho_fe"),
    carbon = c("f_c", "S_cz")
  )

  if (var_name %in% unit_map$length) {
    return(units_choices$length)
  }
  if (var_name %in% unit_map$density) {
    return(units_choices$density)
  }
  if (var_name %in% unit_map$carbon) {
    return(units_choices$carbon)
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
get_right_date_format <- function(x, type){

  original_dates <- as.Date(x, format = type)

  corrected_dates <- ifelse(is.na(original_dates), NA,
                           format(original_dates, "%d/%m/%Y")
                           )


  return(corrected_dates)

}
