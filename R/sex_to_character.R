#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
sex_to_character <- function(x) {
  c("0" = "FEMALE", "1" = "MALE", "2" = "FEMALE")[as.character(x)]
}
