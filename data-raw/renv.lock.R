options("repos" = list(CRAN = "https://cran.r-project.org"))
renv::init(bare = TRUE, force = TRUE)
renv::install(".", prompt = FALSE)
renv::snapshot(lockfile = "inst/setup/renv.lock", type = "explicit", force = TRUE)
