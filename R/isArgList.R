
#' isListArg
#'
#' @param x obj of class `call` or `name``
#' @param envir environment in wchich
#'
#' @return logical
#' @export
isListArg <- function(x, envir) {
  
  # stopifnot(class(x) %in% c("call", "name"))
  
  p.envir <- parent.frame()
  
  sb <- if (!class(x) == "call") substitute(x, env = p.envir) else x
  
  if (sb[[1]] == "$") {
    ## because dollar is first:
    # Browse[4]> sb
    # z$x
    # Browse[4]> as.character(sb)
    # [1] "$" "z" "x"
    is.list(get(as.character(sb[[2]]), envir = envir))
  } else {
    FALSE
  }
}

if (FALSE) {
  debugonce(isArgList)
  isArgList(res[[3]], gp.env)
}



#' parseList
#'
#' @param x 
#' @param envir environment
#'
#' @return vector
#' @export
#'
parseList <- function(x, envir) {
  p.envir <- parent.frame()
  
  sb <- if (!class(x) == "call") substitute(x, env = p.envir) else x
  
  parsed <- as.character(deparse(sb))
  
  unlist(strsplit(parsed, "\\$"))
}
