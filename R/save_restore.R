#' saveCache
#'
#' Save object to rds
#'
#' @param x 
#' @param plan 
#' @param workers 
#' @param promises.env 
#' @param logger.name 
#'
#' @importFrom future %<-% resolve resolved futureOf plan availableCores
#'
#' @return
#' @export
#'
saveCache <-
  function(x,
           path,
           promises.env,
           sufix,
           prefix = "cachemer",
           plan = "multiprocess",
           workers = future::availableCores() - 1,
           logger.name = "test.logger",
           future.plan = "multiprocess") {
    
    # stopifnot(!missing(x))
    stopifnot(!missing(x))
    stopifnot(!missing(path))
    stopifnot(!missing(promises.env))
    
    if (missing(sufix))
      sufix <- as.numeric(Sys.time())
    
    fname <- paste0(prefix, "_", sufix, ".rds")
    
    stopifnot(!file.exists(file.path(path, fname)))
    
    # quote regarding to saving cache to file
    promises.env$fname <- fname
    promises.env$path <- path
    promises.env$x <- x
    
    qt <- quote({
      fresult %<-% {
        saveRDS(x, file = file.path(path, fname))
      }
    })
    
    # future of future variable
    fof <- tryCatch({
      future::futureOf(promises.env$fresult)
    }, error = function(e) {
      flog.debug("'fresult' does not exists", name = logger.name)
      NULL
    })
    
    if (is.null(fof)) {
      # if does not exists then procees and create
      flog.debug("Evaluating promise...", name = logger.name)
      eval(qt, envir = promises.env)
      FALSE
    } else {
      # exists so if not resolved then wait
      fof <- future::futureOf(promises.env$fresult)
      
      if (!future::resolved(fof)) {
        flog.debug("Wait until previous process is finished...",
                   name = logger.name)
        future::resolve(fof)
        flog.debug("Done", name = logger.name)
        TRUE
      } else {
        flog.debug("Evaluating promise...", name = logger.name)
        eval(qt, envir = promises.env)
        FALSE
      }
    }
  }

if (FALSE) {
  promises.env <- new.env()
  var <- list(a = 1, b = 2, c = list(e=2, 241234))
  debugonce(saveCache)
  res <- saveCache(var, path = "dev/", promises.env = promises.env)
  
}
