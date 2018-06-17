
#' is_parenthesized
#' 
#' Determine whether an non-evaluated call is parenthesized. It is wrapper 
#' around unexported function \code{magrittr:::is_parenthesized}
#' 
#' @param expr 
#'
#' @return logical - TRUE if expression is parenthesized, FALSE otherwise.
#' @export
#' @importFrom utils getFromNamespace
is_parenthesized <- function(expr) {
  is_parenthesized <- utils::getFromNamespace("is_parenthesized", "magrittr")
  is_parenthesized(expr)
}
