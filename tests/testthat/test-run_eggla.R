set.seed(2705)
options(digits = 4, scipen = 10)

data("bmigrowth")
library(future)
library(future.apply)
plan(sequential)

test_that("run_eggla", {
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
    random_complexity = "1", # simple to speed-up test
    use_car1 = TRUE,
    parallel = FALSE, # to parallelise Daymont QC
    parallel_n_chunks = 1, # to parallelise Daymont QC
    working_directory = tempdir(),
    quiet = TRUE
  )

  expect_equal(
    sort(basename(result_lmm)),
    sort(c("female.zip", "male.zip"))
  )

  if (Sys.info()[["sysname"]] == "Linux") {
    output_path <- file.path(tempdir(), "eggla")
    dir.create(output_path, showWarnings = FALSE)
    result_gwas <- run_eggla_gwas(
      data = data.table::as.data.table(bmigrowth)[
        j = sex := c("0" = "f", "1" = "m")[as.character(sex)],
      ],
      results = result_lmm,
      id_column = "ID",
      traits = c("slope_.*", "auc_.*", "^AP_.*", "^AR_.*"),
      covariates = c("sex"),
      vcfs = list.files(
        path = system.file("vcf", package = "eggla"),
        pattern = "\\.vcf$|\\.vcf.gz$",
        full.names = TRUE
      ),
      vep = NULL,
      working_directory = output_path,
      bin_path = list(
        bcftools = "/usr/bin/bcftools",
        plink2 = system.file("bin", "plink2", package = "eggla")
      ),
      threads = 4
    )
    unlink(output_path, recursive = TRUE)
    expect_equal(
      sort(basename(result_gwas)),
      sort(c(
        "AR_bmi.glm.linear-gwas.csv.gz", "AP_bmi.glm.linear-gwas.csv.gz",
        "auc_1.5--3.5.glm.linear-gwas.csv.gz", "auc_0--0.5.glm.linear-gwas.csv.gz",
        "auc_6.5--10.glm.linear-gwas.csv.gz", "auc_12--17.glm.linear-gwas.csv.gz",
        "AP_ageyears.glm.linear-gwas.csv.gz", "slope_0--0.5.glm.linear-gwas.csv.gz",
        "slope_1.5--3.5.glm.linear-gwas.csv.gz", "slope_12--17.glm.linear-gwas.csv.gz",
        "slope_6.5--10.glm.linear-gwas.csv.gz", "AR_ageyears.glm.linear-gwas.csv.gz",
        "male.zip", "female.zip", "gwas-software.txt"
      ))
    )

    output_path <- file.path(tempdir(), "eggla")
    dir.create(output_path, showWarnings = FALSE)
    expect_error(run_eggla_gwas(
      data = data.table::as.data.table(bmigrowth)[
        j = sex := c("0" = "f", "1" = "m")[as.character(sex)],
      ],
      results = result_lmm,
      id_column = "ID",
      traits = c("slope_.*", "auc_.*", "^AP_.*", "^AR_.*"),
      covariates = c("sex"),
      vcfs = list.files(
        path = system.file("vcf", package = "eggla"),
        pattern = "\\.vcf$|\\.vcf.gz$",
        full.names = TRUE
      ),
      vep = NULL,
      working_directory = output_path,
      bin_path = list(
        bcftools = "/fake/path/bcftools",
        plink2 = system.file("bin", "plink2", package = "eggla")
      ),
      threads = 4
    ))
    unlink(output_path, recursive = TRUE)

    output_path <- file.path(tempdir(), "eggla")
    dir.create(output_path, showWarnings = FALSE)
    expect_error(run_eggla_gwas(
      data = data.table::as.data.table(bmigrowth)[
        j = sex := c("0" = "f", "1" = "m")[as.character(sex)],
      ],
      results = result_lmm,
      id_column = "ID",
      traits = c("slope_.*", "auc_.*", "^AP_.*", "^AR_.*"),
      covariates = c("sex"),
      vcfs = list.files(
        path = system.file("vcf", package = "eggla"),
        pattern = "\\.vcf$|\\.vcf.gz$",
        full.names = TRUE
      ),
      vep = NULL,
      working_directory = output_path,
      bin_path = list(
        bcftools = "/usr/bin/bcftools",
        plink2 = "/fake/path/plink2"
      ),
      threads = 4
    ))
    unlink(output_path, recursive = TRUE)

    output_path <- file.path(tempdir(), "eggla")
    dir.create(output_path, showWarnings = FALSE)
    result_gwas <- run_eggla_gwas(
      data = as.data.frame(
        data.table::as.data.table(bmigrowth)[
          j = sex := c("0" = "f", "1" = "m")[as.character(sex)],
        ]
      ),
      results = sub("\\.zip$", "", run_eggla_lmm(
        data = bmigrowth,
        id_variable = "ID",
        age_days_variable = NULL,
        age_years_variable = "age",
        weight_kilograms_variable = "weight",
        height_centimetres_variable = "height",
        sex_variable = "sex",
        covariates = NULL,
        male_coded_zero = FALSE,
        random_complexity = "auto", # simple to speed-up test
        use_car1 = TRUE,
        parallel = FALSE, # to parallelise Daymont QC
        parallel_n_chunks = 1, # to parallelise Daymont QC
        working_directory = tempdir(),
        quiet = TRUE,
        clean = FALSE
      )),
      id_column = "ID",
      traits = c("slope_.*", "auc_.*", "^AP_.*", "^AR_.*"),
      covariates = c("sex"),
      vcfs = list.files(
        path = system.file("vcf", package = "eggla"),
        pattern = "\\.vcf$|\\.vcf.gz$",
        full.names = TRUE
      ),
      vep = NULL,
      working_directory = output_path,
      use_info = TRUE,
      bin_path = list(
        bcftools = "/usr/bin/bcftools",
        plink2 = system.file("bin", "plink2", package = "eggla")
      ),
      threads = 4
    )
    unlink(output_path, recursive = TRUE)
    expect_equal(
      sort(basename(result_gwas)),
      sort(c(
        "AR_bmi.glm.linear-gwas.csv.gz", "AP_bmi.glm.linear-gwas.csv.gz",
        "auc_1.5--3.5.glm.linear-gwas.csv.gz", "auc_0--0.5.glm.linear-gwas.csv.gz",
        "auc_6.5--10.glm.linear-gwas.csv.gz", "auc_12--17.glm.linear-gwas.csv.gz",
        "AP_ageyears.glm.linear-gwas.csv.gz", "slope_0--0.5.glm.linear-gwas.csv.gz",
        "slope_1.5--3.5.glm.linear-gwas.csv.gz", "slope_12--17.glm.linear-gwas.csv.gz",
        "slope_6.5--10.glm.linear-gwas.csv.gz", "AR_ageyears.glm.linear-gwas.csv.gz",
        "male", "female", "gwas-software.txt"
      ))
    )
  }
})
