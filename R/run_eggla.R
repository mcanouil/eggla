#' run_eggla
#'
#' @param phenotypes something
#' @param trait something
#' @param cohort_name  something
#' @param output_directory something
#' @param ... Parameters to pass to `rmarkdown::render()`.
#'
#' @return NULL
#' @export
run_eggla <- function(
  phenotypes = NULL,
  trait = "bmi",
  cohort_name = "cohort",
  output_directory = tempdir(),
  ...
) {
  if (is.null(phenotypes) || (!grepl("\\.csv", phenotypes) && !inherits(x = phenotypes, what = "data.frame"))) {
    stop('"phenotypes" must be either a path to a csv file or inherits from "data.frame" class.')
  }

  invisible(file.copy(
    from = system.file("rmarkdown", "templates", "eggla", "skeleton", "skeleton.Rmd", package = "eggla"),
    to = file.path(output_directory, "eggla.Rmd"),
    overwrite = TRUE
  ))

  if (!all(sapply(
    X = strsplit(gsub("\\([^()]+\\)|\n| ", "", utils::packageDescription("eggla", fields = "Suggests")), ",")[[1]],
    FUN = function(x) nchar(system.file(package = x)) > 0
  ))) {
    stop("Packages listed in 'Suggests' field are needed to run this function.")
  }

  invisible(
    rmarkdown::render(
      input = file.path(output_directory, "eggla.Rmd"),
      # output_file = output_file,
      # output_dir = output_directory,
      params = list(
        cohort_name =  cohort_name,
        phenotypes = phenotypes,
        trait = trait,
        output_directory = output_directory
      ),
      ...
    )
  )
}
