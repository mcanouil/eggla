#!/usr/bin/env Rscript

parent_directory <- "/tmp"
project_directory <- file.path(parent_directory, "egg_analysis")
dir.create(project_directory, recursive = TRUE)
setwd(project_directory)
temp_library <- file.path(parent_directory, "R")
dir.create(temp_library, recursive = TRUE)
install.packages("renv", lib = temp_library, repos = "http://cloud.r-project.org")
library("renv", lib.loc = temp_library)
init()
install("mcanouil/eggla@v0.3.0")
