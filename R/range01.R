#' Standardize variables 0 to 1
#'
#' This function will standardize your variables from 0 to 1.
#' @param
#' @keywords standardization
#' @export
#' @examples
#' range01()

range01 <- function(x){(x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))}
