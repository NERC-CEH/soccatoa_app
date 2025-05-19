

##### TEST INPUT #########
dt_soilc <- readr::read_csv("/data/notebooks/rstudio-madtigsoccatoa/soccatoa/data-raw/files/dt_soilc.csv")


# Define the function
convert_coordinates <- function(dt) {
  # Convert the tibble to sf object to easily handle spatial transformations
  # First, check if coordinates are in BNG or WGS84
  dt <- dt %>%
    dplyr::mutate(
      coordinate_system = ifelse(lon >= -180 & lon <= 180 & lat >= -90 & lat <= 90,
                                 "WGS84",
                                 "BNG")
    )

  # Now, handle coordinate transformation
  # Create an sf object only for BNG coordinates and convert them to WGS84
  dt <- dt %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      lon_wgs84 = ifelse(coordinate_system == "BNG", {
        # Transform BNG (EPSG: 27700) to WGS84 (EPSG: 4326)
        point_bng <- sf::st_point(c(lon, lat))
        point_bng_sf <- sf::st_sfc(point_bng, crs = 27700)
        transformed_point <- sf::st_transform(point_bng_sf, crs = 4326)
        sf::st_coordinates(transformed_point)[1]  # Extract transformed lon
      }, lon),

      lat_wgs84 = ifelse(coordinate_system == "BNG", {
        point_bng <- sf::st_point(c(lon, lat))
        point_bng_sf <- sf::st_sfc(point_bng, crs = 27700)
        transformed_point <- sf::st_transform(point_bng_sf, crs = 4326)
        sf::st_coordinates(transformed_point)[2]  # Extract transformed lat
      }, lat)
    ) %>%
    dplyr::ungroup()  # Ensure no grouping issues

  return(dt)
}

dt_soilc_converted <- convert_coordinates(dt_soilc)

dt_soilc_converted <- dt_soilc_converted[, -c(5:6)]


colnames(dt_soilc_converted)[which(colnames(dt_soilc_converted) == "lon_wgs84")] <- "lon"
colnames(dt_soilc_converted)[which(colnames(dt_soilc_converted) == "lat_wgs84")] <- "lat"

dt_soilc_converted <- dplyr::filter(dt_soilc_converted,
                                    survey %in% c("Easter Bush", "ELUM", "RAC", "Ward"))


write.csv(dt_soilc_converted,
          file = "/data/notebooks/rstudio-madtigsoccatoa/soccatoa/data-raw/files/soccatoa_input_2.csv",
          row.names = FALSE)


# dt_soilc <- dplyr::filter(dt_soilc,
#                           survey == "Easter Bush",
#                           year == 2004)


