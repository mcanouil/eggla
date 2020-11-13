#' Title
#'
#' @param fit
#'
#' @return
#' @export
#'
#' @examples
format_lme <- function(fit) {
  res_tbl <- tidy(fit)
  res_tbl[["term"]] <- gsub("(gsp)\\(.*\\)\\)|(poly)\\(.*, degree = .\\)", "\\1\\2(...)", res_tbl[["term"]])
  res_tbl[["effect"]] <- ifelse(
    test = res_tbl[["group"]] %in% c("Residual", "fixed"),
    yes = toupper(res_tbl[["group"]]),
    no = gsub("(.*)", "RANDOM (\\1)", res_tbl[["group"]])
  )
  res_tbl[["group"]] <- NULL
  res_tbl[["df"]] <- ifelse(
    test = is.na(res_tbl[["df"]]),
    yes = "---",
    no = format(res_tbl[["df"]], digits = 0, nsmall = 0, trim = TRUE, big.mark = ",")
  )
  res_tbl[] <- lapply(res_tbl, function(x) {
    if (is.numeric(x)) {
      ifelse(is.na(x), "---", format(x, digits = 3, nsmall = 3, trim = TRUE))
    } else {
      ifelse(is.na(x), "---", x)
    }
  })
  names(res_tbl) <- toupper(names(res_tbl))
  res_tbl
}
