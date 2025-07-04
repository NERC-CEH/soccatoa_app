## code to prepare `database_accounts` dataset goes here
example_user <- data.frame(
  "username" = digest::digest("test", algo = "md5"),
  "password" = digest::digest("test", algo = "md5"),
  "use" = "research",
  "date_registered" = Sys.Date()
)

database_accounts <- example_user

usethis::use_data(database_accounts, overwrite = TRUE)
