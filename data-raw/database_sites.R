## code to prepare `database_sites` dataset goes here

#this created an empty dataset to start off. As the tool it is used this DB is populated
database_sites <- data.frame(
  "survey" = NA, # survey
  "site_id" = NA, # a unique site code
  "day" = NA,
  "month" = NA,
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


usethis::use_data(database_sites, overwrite = TRUE)
