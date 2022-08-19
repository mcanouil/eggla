bmigrowth <- read.csv("data-raw/bmigrowth.csv.gz")
bmigrowth[["ID"]] <- sprintf("%03d", bmigrowth[["ID"]])
usethis::use_data(bmigrowth, overwrite = TRUE)



library(data.table)
library(future)
library(future.apply)
vcfs <- list.files("vcf", "\\.vcf.gz$", full.names = TRUE)
phenotypes <- "_targets/user/phenotypes.csv"
selected_ids <- fread(phenotypes)[
  j = sample(unique(ID), 50),
  by = "sex"
][
  order(sex)
][
  j = list(
    old = V1,
    new = setDT(bmigrowth)[order(sex), unique(.SD), .SDcols = c("ID", "sex")][["ID"]]
  )
]
data.table::fwrite(
  x = selected_ids,
  file = "inst/samples_to_rename.txt",
  col.names = FALSE,
  sep = " "
)
data.table::fwrite(
  x = selected_ids[j = list(new)],
  file = "inst/samples_to_keep.txt",
  col.names = FALSE,
  sep = " "
)
dir.create(file.path("inst", "vcf"), showWarnings = FALSE)
plan(multicore, workers = 22)
message(sprintf("Number of workers: %d", nbrOfWorkers()))
out_vcf_filter <- future.apply::future_sapply(
  X = vcfs,
  FUN = function(x) {
    regions_dt <- fread(
      cmd = paste(
        "/usr/bin/bcftools",
          "reheader", x,
          "--samples inst/samples_to_rename.txt",
        "|",
        "/usr/bin/bcftools",
        "view",
        "--exclude 'INFO/INFO < 0.8'",
        "--min-alleles 2 --max-alleles 2 --types snps",
        "--force-samples",
        "--samples-file inst/samples_to_keep.txt",
        "--private",
        "|",
        "/usr/bin/bcftools",
        "query",
        "-f '%CHROM\t%POS\t%INFO/AC\t%INFO/INFO\n'"
      )
    )[V4 >= 0.8][order(V3, decreasing = TRUE), .SD[1:100]]
    cmd <- paste(
      "/usr/bin/bcftools",
        "reheader", x,
        "--samples inst/samples_to_rename.txt",
      "|",
      "/usr/bin/bcftools",
        "view",
        "--force-samples",
        "--samples-file inst/samples_to_keep.txt",
        "--private",
        "--targets", paste(paste0(regions_dt[[1]], ":", regions_dt[[2]]), collapse = ","),
        # "--targets-overlap 2",
        "--output-type z --output",
        file.path("inst", "vcf", basename(x))
    )
    system(cmd)
  }
)
unlink("inst/samples_to_rename.txt")
unlink("inst/samples_to_keep.txt")
