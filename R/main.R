

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
      private$shared$env[[nm]] <- val
    },
    getShared = function() function(nm) {
      private$shared$env[[nm]]
    }
  )
)

cacherRef <- R6Class("cacherEnv",
                     inherit = cacher)
