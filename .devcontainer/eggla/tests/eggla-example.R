#!/usr/bin/env Rscript

set.seed(2705)

library(eggla)
library(data.table)

data("bmigrowth")

result_lmm <- run_eggla_lmm(
  data = bmigrowth,
  id_variable = "ID",
  age_days_variable = NULL,
  age_years_variable = "age",
  weight_kilograms_variable = "weight",
  height_centimetres_variable = "height",
  sex_variable = "sex",
  covariates = NULL,
  male_coded_zero = FALSE,
  random_complexity = 1, # simple to speed-up test
  use_car1 = TRUE,
  parallel = FALSE, # to parallelise Daymont QC
  parallel_n_chunks = 1, # to parallelise Daymont QC
  working_directory = tempdir(),
  quiet = FALSE,
  clean = FALSE
)

output_path <- file.path(tempdir(), "eggla")
dir.create(output_path, showWarnings = FALSE)

result_gwas <- run_eggla_gwas(
  data = as.data.table(bmigrowth)[
    j = sex := c("0" = "f", "1" = "m")[as.character(sex)],
  ],
  results = result_lmm,
  id_column = "ID",
  traits = c("slope_.*", "auc_.*", "^AP_.*", "^AR_.*"),
  covariates = c("sex", sprintf("PC%02d", 1:5)),
  vcfs = list.files(
    path = system.file("vcf", package = "eggla"),
    pattern = "\\.vcf$|\\.vcf.gz$",
    full.names = TRUE
  ),
  working_directory = output_path,
  use_info = TRUE,
  bin_path = list(
    bcftools = "/usr/bin/bcftools",
    plink2 = system.file("bin", "plink2", package = "eggla")
  ),
  bcftools_view_options = NULL,
  build = "38",
  strand = "+",
  info_type = "IMPUTE2 info score via 'bcftools +impute-info'",
  threads = 1,
  quiet = FALSE,
  clean = TRUE
)

unlink(output_path, recursive = TRUE)
