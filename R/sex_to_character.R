#' sex_to_character
#'
#' @param x something
#'
#' @return something
#' @export
sex_to_character <- function(x) {
  c("0" = "FEMALE", "1" = "MALE", "2" = "FEMALE")[as.character(x)]
}
