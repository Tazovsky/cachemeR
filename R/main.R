# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello <- function() {
  print("Hello, world!")
}



install.packages("pipeR")

library(pipeR)
pipeR::pipeline()

pipeline(1:10, sin, sum)

library(dplyr)



testFun <- function(x = 1:10) {
  browser()
  x %>% sin %>% sum

}

testFun()

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


spr %<-c% testFun()

install.packages("listenv")

listenv::listenv()
listenv::get_variable()

yaml::write_yaml(list(path = path))


library(R6)

casher <- R6Class(
  "casher",
  public = list(
    path = NULL,
    created_at = NULL,
    overwrite = NULL,
    count = NULL,
    initialize = function(path = NA, overwrite = FALSE, env = new.env()) {

      if (!grepl(".*\\.yaml$|.*\\.yml$", path))
        stop("File has no 'yml' or 'yaml' extension.")

      if (overwrite == FALSE && file.exists(path))
        stop("File already exists. Please set 'overwrite' parameter to overwrite.")

      created_at <- Sys.time()

      yaml::write_yaml(x = list(
        created_at = as.character(created_at),
        created_at_ts = as.character(as.integer(created_at))
      ), file = path)

      self$overwrite <- overwrite
      self$created_at <- created_at
      self$path <- path
      private$shared$env <- env
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
    shared = list(
      env = NULL
    )
  ),
  active = list(
    # setEnv = function(e) {
    #   private$shared$env <- e
    # },
    getEnv = function() {
      private$shared$env
    },
    share = function() function(nm, val) {

      # if (nm  %in% names(env))
      #   cat("Overwriting", nm, "variable.")

      private$shared$env[[nm]] <- val
    },
    getShared = function() function(nm) {
      private$shared$env[[nm]]
    }
  )
)

obj <- casher$new(path = "config.yaml", overwrite = TRUE)

obj$getEnv
obj$share(nm = "test", val = 1234)
obj$getEnv
obj$share( "test", 1233)
obj$getShared("test")


obj2 <- casher$new(path = "config.yaml", overwrite = TRUE)

obj2$getShared("test")
obj2$share(nm = "test", val = 4321)
obj2$getShared("test")



casherEnv <- R6Class("casherEnv",
                     inherit = casher,
                     public = list(
                       summary = function(...) {
                         self$summary()
                       }
                     )
)

x <- casherEnv$new("config.yaml", overwrite = TRUE)
x$add()
x$path()
x$count

class(x)



casherEnv$summary()



obj1 <- casher$new("config.yaml", overwrite = TRUE)

obj1$summary()
obj1$add()


initCacheR <- function(path, overwrite = TRUE) {

  if (missing(path))
    stop("Provide 'path' argument.")

  cacher.env <- new.env()

  obj <- casher$new(path = path, overwrite = overwrite)

  assign("casher", obj, envir = cacher.env)

  options("cacher.env" = cacher.env)

  TRUE
}


initCacheR("config.yaml")


getCacheRobj <- function() {

  env <- getOption("cacher.env")

  if (is.null(env$casher))
    stop("'casher' object not found")

  env$casher
}

setCacheRobj <- function(obj) {

  if (!inherits(obj, "casher"))
    stop("Is not 'casher' class")

  env <- getOption("cacher.env")

  assign("casher", obj, envir = cacher.env)

  TRUE
}






