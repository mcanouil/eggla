bmigrowth <- read.csv("data-raw/bmigrowth.csv.gz")
bmigrowth[["ID"]] <- sprintf("%03d", bmigrowth[["ID"]] )
usethis::use_data(bmigrowth, overwrite = TRUE)
