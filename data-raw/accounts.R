## code to prepare `accounts` dataset goes here
example_user <- data.frame("username" = digest::digest("test", algo = "md5"),
                           "password" = digest::digest("test", algo = "md5"),
                           "use" = "research",
                           "date_registered" = Sys.Date()
                           )

accounts <- example_user

usethis::use_data(accounts, overwrite = TRUE)
