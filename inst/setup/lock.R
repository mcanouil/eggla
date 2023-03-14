pkgs <- setdiff(rownames(old.packages(repos = "https://cloud.r-project.org/")), "pak")
if (length(pkgs) > 0) pak::pkg_install(pkgs)

lock_file <- "inst/setup/pkg.lock"
pak::lockfile_create(
  pkg = c(".", "data.table", "devtools", "languageserver", "httpgd", "svglite", "ragg"),
  lockfile = lock_file,
  lib = NULL,
  upgrade = TRUE,
  dependencies = TRUE
)
lock_file_json <- jsonlite::fromJSON(readLines(lock_file))
lock_file_json[["packages"]] <- lock_file_json[["packages"]][!lock_file_json[["packages"]][["ref"]] %in% "local::.", ]
writeLines(jsonlite::toJSON(lock_file_json, pretty = TRUE, auto_unbox = TRUE), lock_file)
usethis::use_latest_dependencies(TRUE)
