#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

library(eggla)

data("bmigrowth")
run_eggla(
  data = bmigrowth,
  id_variable = "ID",
  age_days_variable = NULL,
  age_years_variable = "age",
  weight_kilograms_variable = "weight",
  height_centimetres_variable = "height",
  sex_variable = "sex",
  covariates = NULL,
  male_coded_zero = FALSE,
  random_complexity = 2,
  use_ar1 = TRUE,
  parallel = FALSE, # to parallelise Daymont QC
  parallel_n_chunks = 1, # to parallelise Daymont QC
  working_directory = tempdir()
)
