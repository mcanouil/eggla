#!/usr/bin/env Rscript

parent_directory <- "/tmp"
data_path <- ""
data <- fread(data_path)[
  j = `:=`(
    "agedays" = floor(age * 365.25), # convert age in years to age in days and as integers ...
    "WEIGHTKG" = as.numeric(weight),
    "HEIGHTCM" = as.numeric(height)
  )
][
  j = `:=`(# recode sex with Male = 0 and Female = 1 ...
    "sex_daymont" = c("0" = "1", "1" = "0")[as.character(sex)]
    # "sex_daymont" = as.character(sex) # if already coded with Male = 0 and Female = 1
  )
]

project_directory <- file.path(parent_directory, "egg_analysis")
dir.create(project_directory, recursive = TRUE)
setwd(project_directory)
temp_library <- file.path(parent_directory, "R")
dir.create(temp_library, recursive = TRUE)
install.packages("renv", lib = temp_library, repos = "http://cloud.r-project.org")
library("renv", lib.loc = temp_library)
init()
install("mcanouil/eggla@v0.3.0")
restore(lockfile = system.file("renv.lock", package = "eggla"))
