#' @title cachemer
#' @description TODO
#'
#' @importFrom R6 R6Class
#' @importFrom yaml read_yaml write_yaml
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} with methods for caching objects
#' @format \code{\link{R6Class}} object.
#' @examples \dontrun{
#'
#' }
#'
#' @field new(path="config.yaml",overwrite=TRUE,env=new.env()) Initializes \code{\link{R6Class}} object
#' @field summary Prints summary of \code{\link{R6Class}} object
#' @field getEnv Gets environment shared between multiple `cachemer` objects
#' @field share(nm,val) Shares object between multiple `cachemer` objects
#' @field getShared(nm) Gets shared object
#' @field cacheme(fun.name,fun.body,arguments,output=NULL,algo="md5") Caches object/function
#' @field lastCache Gets last object added to cache
cachemer <- R6::R6Class(
  "cacher",
  public = list(
    path = NULL,
    created_at = NULL,
    overwrite = NULL,
    count = NULL,
    initialize = function(path = "config.yaml", overwrite = TRUE, env = new.env()) {

      if (is.null(private$shared$path)) {

        if (!grepl(".*\\.yaml$|.*\\.yml$", path))
          stop("File has no 'yml' or 'yaml' extension.")

        if (overwrite == FALSE && file.exists(path))
          stop("File already exists. Please set 'overwrite' parameter to overwrite.")

        created_at <- Sys.time()

        if (!dir.exists(dirname(path)))
          dir.create(dirname(path))

        yaml::write_yaml(x = list(
          created_at = as.character(created_at),
          created_at_ts = as.character(as.integer(created_at))
        ), file = path)


        self$path <- path
        private$shared$path <- path
        self$overwrite <- overwrite
        self$created_at <- created_at
      } else {
        self$path <- private$shared$path
        yml <- yaml::read_yaml(self$path)
        self$overwrite <- yml$created_at
      }

    },
    add = function() {
      if (is.null(self$count))
        self$count <- 1
      else
        self$count <- self$count + 1
    },
    summary = function() {

      cat("path:", self$path, "\n")
      cat("created at:", as.character(self$created_at), "\n")
      cat("count:", self$count, "\n")
    }
  ),
  private = list(
    # great tips: https://elvinouyang.github.io/study%20notes/oop-with-r-s3-and-r6/ #nolint
    shared = {
      e <- new.env()
      e$envir <- e
      e$path <- NULL
      e$cache <- NULL
      e$last.cache <- NULL
      e
    }
  ),
  active = list(
    getEnv = function() {
      private$shared$envir
    },
    share = function() function(nm, val) {
      private$shared$envir[[nm]] <- val
    },
    getShared = function() function(nm) {
      private$shared$envir[[nm]]
    },
    lastCache = function() {
      private$shared$last.cache
    }
  )
)


cachemer$set("public", "cacheme", function(fun.name,
                                         fun.body,
                                         arguments,
                                         output = NULL,
                                         algo = "md5") {

  if (is.null(private$shared$cache))
    private$shared$cache <- list()

  if (any(sapply(list(fun.name, fun.body, arguments, output),
                 function(x) missing(x))))
      stop("Provide all arguments: fun.name, fun.body, arguments, output.")

  stopifnot(inherits(output, "call"))

  obj2cache <- list(
    arguments = arguments,
    # pass output only when it is clear that
    # current fun.name and args are cached
    output = NULL,
    hash = digest::digest(list(fun.name, fun.body, arguments), algo = algo))

  if (is.null(private$shared$cache[[obj2cache$hash]])) {
    flog.info(sprintf("Caching '%s' for first time...", fun.name))

    obj2cache$output <- eval(output)

    private$shared$cache[[obj2cache$hash]] <- obj2cache

    # update last cache
    private$shared$last.cache <- obj2cache

  } else if (!is.null(private$shared$cache[[obj2cache$hash]])) {
    flog.info(sprintf("'%s' is already cached...", fun.name))

    # update last cache
    private$shared$last.cache <- private$shared$cache[[obj2cache$hash]]

  } else {
    flog.info(sprintf("Caching '%s'...", fun.name))

    # something has changed in arguments so need to retrieve outpu
    obj2cache$output <- eval(output)

    # cache
    private$shared$cache[[obj2cache$hash]] <- obj2cache

    # update last cache
    private$shared$last.cache <- obj2cache
  }

})


#' @title ref
#' @description Inherits from \link[cachemeR]{cacher}
#'
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} with methods for caching objects
#' @format \code{\link{R6Class}} object.
cachemerRef <- R6Class("cachemerRef",
               inherit = cachemer)



