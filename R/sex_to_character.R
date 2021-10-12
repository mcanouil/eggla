#' sex_to_character
#'
#' @param x A vector of numeric to be converted to character.
#'
#' @return A vector of character.
#' @export
sex_to_character <- function(x) {
  c("0" = "FEMALE", "1" = "MALE", "2" = "FEMALE")[as.character(x)]
}
