renv::snapshot(
  lockfile = "inst/setup/renv.lock",
  packages = c(".", "data.table"),
  library = .libPaths(),
  force = TRUE,
  prompt = FALSE
)

lock_file <- "inst/setup/pkg.lock"
pak::lockfile_create(
  pkg = c(".", "data.table"),
  lockfile = lock_file,
  lib = NULL,
  upgrade = FALSE,
  dependencies = TRUE
)
lock_file_json <- jsonlite::fromJSON(readLines(lock_file))
lock_file_json[["packages"]] <- lock_file_json[["packages"]][!lock_file_json[["packages"]][["ref"]] %in% "local::.", ]
writeLines(jsonlite::toJSON(lock_file_json, pretty = TRUE, auto_unbox = TRUE), lock_file)
