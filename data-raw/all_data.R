## code to prepare `all_data` dataset goes here

all_data <- data.frame(
  "survey" = NA, # survey
  "site_id" = NA, # a unique site code
  "year" = NA, # or date "YYYY-mm-dd"
  "lon" = NA,
  "lat" = NA,
  "z" = NA,
  "d" = NA,
  "rho_fe" = NA,
  "f_c" = NA,
  "S_cz" = NA,
  "user" = NA
)


usethis::use_data(all_data, overwrite = TRUE)
