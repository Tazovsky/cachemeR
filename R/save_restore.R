#' saveCache
#'
#' Save object to rds
#'
#' @param x 
#' @param plan 
#' @param workers 
#' @param promises.env 
#' @param logger.name 
#' @param path 
#' @param sufix 
#' @param force.eval logical (default \code{FALSE}); if \code{FALSE} then
#' queue and evaluate saving process in separate process - else wait until
#' saving process is finished
#' @param prefix 
#' @param future.plan 
#'
#' @importFrom future %<-% resolve resolved futureOf plan availableCores
#' @importFrom futile.logger flog.debug
#' @return logical
#' @export
#'
saveCache <-
  function(x,
           path,
           promises.env,
           sufix,
           force.eval = FALSE,
           prefix = "cachemer",
           plan = "multisession",
           workers = future::availableCores() - 1,
           logger.name = "test.logger",
           future.plan = "multisession") {
    
    stopifnot(!missing(x))
    stopifnot(!missing(path))
    stopifnot(!missing(promises.env))
    
    if (missing(sufix))
      sufix <- as.numeric(Sys.time())
    
    plan(future.plan)
    
    fname <- paste0(prefix, "_", sufix, ".rds")
    
    stopifnot(!file.exists(file.path(path, fname)))
    
    if (force.eval) {
      flog.debug("Saving file...", name = logger.name)
      target <- file.path(path, fname)
      saveRDS(x, file = target)
      TRUE
    } else {
      
      # quote regarding to saving cache to file
      promises.env$fname <- fname
      promises.env$path <- path
      promises.env$x <- x
      
      promises.env$fresult <- future::future({
        saveRDS(x, file = file.path(path, fname))
      })
      
      # future of future variable
      fof <- tryCatch({
        future::futureOf(promises.env$fresult)
      }, error = function(e) {
        flog.debug("'fresult' does not exist", name = logger.name)
        NULL
      })

      if (is.null(fof)) {
        # if does not exists then procees and create
        flog.debug("Evaluating promise...", name = logger.name)
        eval(quote_fut, envir = promises.env)
        FALSE
      } else {
        # exists so if not resolved then wait
        fof <- future::futureOf(promises.env$fresult)
        if (!future::resolved(fof)) {
          flog.debug("Wait until previous process is finished...",
                     name = logger.name)
          future::resolve(fof)
          flog.debug("Resolved.", name = logger.name)
          TRUE
        } else {
          flog.debug("Evaluating promise...", name = logger.name)
          future::value(promises.env$fresult)
          FALSE
        }
      }
    }
  }

if (FALSE) {
  promises.env <- new.env()
  var <- list(a = 1, b = 2, c = list(e=2, 241234))
  # debugonce(saveCache)
  res <- saveCache(var, path = "dev/cache/", promises.env = promises.env)
  
}


#' restoreCache
#'
#' @param path 
#' @param sufix 
#' @param prefix 
#' @param plan 
#' @param workers 
#' @param logger.name 
#' @param future.plan 
#' 
#' @importFrom future.apply future_lapply
#' @importFrom future plan
#' 
#' @return list
#' @export
#' @rdname restoreCache
restoreCache <-
  function(path,
           sufix = ".*",
           prefix = "cachemer",
           plan = "multisession",
           workers = future::availableCores() - 1,
           logger.name = "test.logger",
           future.plan = "multisession") {
    
    stopifnot(!missing(path))
    
    future::plan(future.plan)
    
    files <- list.files(path,
                        pattern = getPattern(prefix, sufix),
                        full.names = TRUE)
    
    flog.debug(sprintf("Restoring cache from: %s", path), name = logger.name)
    
    cache.restored <- future.apply::future_lapply(files, readRDS)
    
    nms <- sapply(cache.restored, function(el) el$hash)
    
    names(cache.restored) <- nms    
    
    # look for duplicates and remove them
    is.dupl <- base::duplicated(names(cache.restored))
    
    if (any(is.dupl)) {
      # get duplicated elements
      dupl.elem <- names(cache.restored)[is.dupl]
      flog.debug(sprintf("Removing %s duplicates", length(dupl.elem)),
                 name = logger.name)
      # find indexes of dulpicated elements
      dupl.idx <- match(dupl.elem, names(cache.restored))
      # rm duplicated element - highest timestamp (ts) will stay
      cache.restored <- cache.restored[-dupl.idx]
    }
    
    return(cache.restored)
  }

#' getPattern
#'
#' @param prefix 
#' @param sufix 
#'
#' @return string
#' @export
#'
#' @rdname restoreCache
getPattern <- function(prefix, sufix = ".*") {
  paste0(".*", prefix, "_", sufix, ".rds")
}
