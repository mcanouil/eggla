#!/usr/bin/env Rscript

parent_directory <- getwd()

setwd(file.path(parent_directory, "egg_analysis"))
renv::activate()

eggla::run_eggla(
  data = data.table::fread("data_file_path"),
  id_variable = "ID",
  age_days_variable = NULL,
  age_years_variable = "age",
  weight_kilograms_variable = "weight",
  height_centimetres_variable = "height",
  sex_variable = "sex",
  covariates = NULL,
  male_coded_zero = FALSE,
  parallel = FALSE,
  parallel_n_chunks = 1,
  working_directory = parent_directory
)
