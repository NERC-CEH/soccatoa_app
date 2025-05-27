## code to prepare `example_output` dataset goes here

dt_soilc <- readr::read_csv(here::here("data-raw/files/dt_soilc.csv"))
dt_soilc <- dplyr::filter(
  dt_soilc,
  survey == "Easter Bush",
  year == 2004
)

colnames(dt_soilc)[5] <- "easting"
colnames(dt_soilc)[6] <- "northing"

example_output <- dt_soilc[which(!is.na(dt_soilc$S_cz)), ]



usethis::use_data(example_output, overwrite = TRUE)
