
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

#' is_pipe
#'
#' @param x 
#'
#' @return logical
#' @export
#'
is_pipe <- function(x) {
  !grepl("^%/%$", x) && !grepl("^%%$", x) && grepl("%.*%", x)
}

#' is_function
#'
#' @param x 
#' @param env 
#'
#' @return logical
#' @export
#'
is_function <- function(x, env) {
  is.call(x) && is.function(get(as.character(x)[1], env)) ||
    is.name(x) && is.function(get(as.character(x), env))
}