# apply(
#   X = desc::description$new()$get_deps()[-1, ],
#   MARGIN = 1,
#   FUN = function(row) {
#     ver <- sub("\\*|>= ", "", row[3])
#     if (nchar(ver) > 0) {
#       paste0(row[2], "@", ver)
#     } else {
#       row[2]
#     }
#   }
# )
options("repos" = list(CRAN = "https://cran.r-project.org"))
renv::snapshot(
  packages = subset(desc::description$new()$get_deps(), type %in% "Imports")[["package"]],
  lockfile = "inst/setup/renv.lock",
  prompt  = FALSE,
  force = TRUE
)
