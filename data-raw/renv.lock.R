renv::init(bare = TRUE, force = TRUE)
renv::snapshot(lockfile = "inst/setup/renv.lock", packages = ".", force = TRUE, prompt = FALSE)
unlink(".Rprofile")
unlink(list.files("renv", include.dirs = TRUE, all.files = TRUE, no.. = TRUE, full.names = TRUE), recursive = TRUE)
