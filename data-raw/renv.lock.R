renv::snapshot(
  lockfile = "inst/setup/renv.lock",
  packages = ".",
  library = .libPaths(),
  force = TRUE,
  prompt = FALSE
)
