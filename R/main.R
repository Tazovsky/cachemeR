#' @title cachemer
#' @description TODO
#'
#' @importFrom R6 R6Class
#' @importFrom yaml read_yaml write_yaml
#' @importFrom futile.logger flog.threshold flog.info
#' @importFrom data.table data.table := rbindlist
#' @importFrom tibble as_tibble
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} with methods for caching objects
#' @format \code{\link{R6Class}} object.
#' @examples \dontrun{
#'  TODO: add examples
#' }
#'
#' @field new(path,overwrite=TRUE) Initializes \code{\link{R6Class}} object
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
    dirname = NULL,
    created_at = NULL,
    overwrite = NULL,
    initialize = function(path, overwrite = TRUE) {
      
      if (private$shared$logger$is.on)
        flog.threshold(INFO, name = private$shared$logger$name)
      else
        flog.threshold(ERROR, name = private$shared$logger$name)
      
      
      if (!missing(path) && !is.null(path)) {

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
        self$dirname <- dirname(path)
        
        private$shared$path <- path
        private$shared$dirname <- dirname(path)
        
        self$overwrite <- overwrite
        self$created_at <- created_at
      } else if (!is.null(private$shared$path)) {
        self$path <- private$shared$path
        
        if (!file.exists(self$path))
          stop(sprintf("Yaml file does not exist"))
        
        yml <- yaml::read_yaml(self$path)
        self$overwrite <- yml$created_at
      } else {
        stop("Missing 'path' argument")
      }

    },
    summary = function(output.class = "tbl") {
      
      stopifnot(output.class %in% c("data.table", "tbl", "tbl_df"))
      
      cached.obj <- private$shared$cache
      
      dt <- data.table::data.table()
      
      if (length(cached.obj) == 0) {
        flog.info("Cache is empty", name = private$shared$logger$name)
      } else {
        flog.info(sprintf("Cache contains %s objects", length(cached.obj)), 
                  name = private$shared$logger$name)
        
        for (i in 1:length(cached.obj)) {
          
          el <- cached.obj[[i]]
          
          dt.from.list <- data.table::data.table(
            id = el$hash,
            output = el$output,
            arguments = as.list(list(el$arguments), all.names = TRUE),
            fun_name_hash = el$hashes$fun.name,
            fun_body_hash = el$hashes$fun.body,
            fun_args_hash = el$hashes$arguments
          )
          
          dt <- data.table::rbindlist(list(dt, dt.from.list))
        }
      }
      
      if (output.class == "data.table")
        dt
      else
        tibble::as_tibble(dt)
    },
    clear = function(all = FALSE) {
      flog.info("Clearing cache", name = private$shared$logger$name)
      obj.names <- names(private$shared)
      exclude <- if (all) c("envir", "logger") else c("envir", "path", "logger")
      obj.names <- obj.names[!obj.names %in% exclude]
      invisible(lapply(obj.names, function(nm) private$shared[[nm]] <- NULL))
    }
  ),
  private = list(
    # great tips: https://elvinouyang.github.io/study%20notes/oop-with-r-s3-and-r6/ #nolint
    shared = {
      e <- new.env()
      e$envir <- e
      e$path <- NULL
      e$dirname <- NULL
      e$cache <- NULL
      e$last.cache <- NULL
      e$logger <- list(name = "cachemer.logger", is.on = FALSE)
      e$save.options <- list(
        prefix = "cachemer",
        plan = "multiprocess",
        env = new.env()
      )
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
    },
    setLogger = function() function(is.on) {
      
      if (missing(is.on))
        stop("'is.on' argument is missing")
      
      stopifnot(is.logical(is.on))
      
      if (is.on) {
        private$shared$logger$is.on <- TRUE
        invisible(flog.threshold(INFO, name = private$shared$logger$name))
        flog.info("Logger is on", name = private$shared$logger$name)
      } else {
        private$shared$logger$is.on <- FALSE
        invisible(flog.threshold(ERROR, name = private$shared$logger$name))
      }
    }
  )
)


cachemer$set("public", "cacheme", function(fun.name,
                                         fun.body,
                                         arguments,
                                         output = NULL,
                                         envir = parent.frame(1),
                                         algo = "md5") {

  set.seed(123)

  if (is.null(private$shared$cache))
    private$shared$cache <- list()

  if (any(sapply(list(fun.name, fun.body, arguments, output),
                 function(x) missing(x))))
      stop("Provide all arguments: fun.name, fun.body, arguments, output.")

  stopifnot(inherits(output, "call"))


  # remove attribs to reproduct hash
  attributes(fun.body) <- NULL

  hashes <- list(
    fun.name = digest::digest(fun.name, algo),
    fun.body = digest::digest(fun.body, algo),
    arguments = digest::digest(arguments, algo)
  )
  
  obj2cache <- list(
    fun.name = fun.name,
    fun.body = fun.body,
    arguments = arguments,
    hashes = hashes,
    # pass output only when it is clear that
    # current fun.name and args are cached
    output = NULL,
    hash = digest::digest(
      list(hashes$fun.name, hashes$fun.body, hashes$arguments),
      algo = algo
    )
  )
  
  if (is.null(private$shared$cache[[obj2cache$hash]])) {
    flog.info(sprintf("Caching '%s' for first time...", fun.name),
              name = private$shared$logger$name)

    obj2cache$output <- evalOutput(output, envir = envir)
    obj2cache$ts <- as.numeric(Sys.time())
    
    # save cache to file
    saveCache(
      x = obj2cache,
      prefix = private$shared$save.options$prefix,
      path = private$shared$dirname,
      promises.env = private$shared$save.options$env,
      future.plan = private$shared$save.options$plan,
      logger.name = private$shared$logger$name
    )
    
    private$shared$cache[[obj2cache$hash]] <- obj2cache

    # update last cache
    private$shared$last.cache <- obj2cache

  } else if (!is.null(private$shared$cache[[obj2cache$hash]])) {
    flog.info(sprintf("'%s' is already cached...", fun.name),
              name = private$shared$logger$name)

    # update last cache
    private$shared$last.cache <- private$shared$cache[[obj2cache$hash]]

  } else {
    flog.info(sprintf("Caching '%s'...", fun.name),
              name = private$shared$logger$name)

    # something has changed in arguments so need to retrieve outpu
    obj2cache$output <- evalOutput(output, envir = envir)

    # cache
    private$shared$cache[[obj2cache$hash]] <- obj2cache

    # update last cache
    private$shared$last.cache <- obj2cache
  }

})


#' @title cachemerRef
#' @description Inherits from \code{\link{cachemer}}
#'
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} with methods for caching objects
#' @format \code{\link{R6Class}} object.
cachemerRef <- R6Class("cachemerRef",
               inherit = cachemer)

