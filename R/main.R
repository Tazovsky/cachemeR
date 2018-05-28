
`%c>%` <- function(lhs, rhs){
  rhs <- substitute(rhs)

  out <- pipe(lhs, rhs, env=parent.frame())

  browser()

  out
}


`%<-c%` <- function(x, value) {
  target <- substitute(x)
  expr <- substitute(value)
  envir <- parent.frame(1)

  browser()

  ls(envir = envir)

}


library(R6)

cacher <- R6Class(
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
    shared = {
      e <- new.env()
      e$envir <- e
      e$path <- NULL
      e
    }
  ),
  active = list(
    # setEnv = function(e) {
    #   private$shared$env <- e
    # },
    getEnv = function() {
      private$shared$envir
    },
    share = function() function(nm, val) {

      if (nm  %in% names(env))
        cat(sprintf("Overwriting '%s' variable.\n", nm))

      private$shared$env[[nm]] <- val
    },
    getShared = function() function(nm) {
      private$shared$env[[nm]]
    }
  )
)

cacherRef <- R6Class("cacherEnv",
                     inherit = cacher)

obj <- cacher$new(path = "config.yaml", overwrite = TRUE)




# =============================================================

obj$getEnv
obj$share(nm = "test", val = 123456)
obj$getShared("test")


obj2 <- cacher$new(path = "config.yaml", overwrite = TRUE)

obj2$getShared("test")
obj2$share(nm = "test", val = 4321)
obj2$getShared("test")
obj$getShared("test")

obj2$getEnv
obj2$getShared("envir")

e <- obj2$getEnv
e$envir



initCacheR <- function(path, overwrite = TRUE) {

  if (missing(path))
    stop("Provide 'path' argument.")

  cacher.env <- new.env()

  obj <- cacher$new(path = path, overwrite = overwrite)

  assign("cacher", obj, envir = cacher.env)

  options("cacher.env" = cacher.env)

  TRUE
}


initCacheR("config.yaml")


getCacheRobj <- function() {

  env <- getOption("cacher.env")

  if (is.null(env$cacher))
    stop("'cacher' object not found")

  env$cacher
}

setCacheRobj <- function(obj) {

  if (!inherits(obj, "cacher"))
    stop("Is not 'cacher' class")

  env <- getOption("cacher.env")

  assign("cacher", obj, envir = cacher.env)

  TRUE
}






