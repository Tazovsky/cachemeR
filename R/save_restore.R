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
    
    plan(future.plan)
    
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
#' @return
#' @export
#' @rdname restoreCache
restoreCache <-
  function(path,
           sufix,
           prefix = "cachemer",
           plan = "multiprocess",
           workers = future::availableCores() - 1,
           logger.name = "test.logger",
           future.plan = "multiprocess") {
    
    stopifnot(!missing(path))
    
    if (missing(sufix))
      sufix <- ".*"
    
    future::plan(future.plan)
    
    files <- list.files(path,
                        pattern = getPattern(prefix, sufix),
                        full.names = TRUE)
    
    flog.debug(sprintf("Restoring cache from: %s", path))
    
    cache.restored <- future.apply::future_lapply(files, readRDS)
    
    nms <- sapply(cache.restored, function(el) el$hash)
    
    names(cache.restored) <- nms    
    
    return(cache.restored)
  }

if (FALSE) {
  
  res <- restoreCache("dev/cache/file12c8332ecd202/")
  
  testthat::context("restoreCache")
  
  testthat::test_that("restoreCache: returned object", {
    dir.create(tmp.dir <- tempfile())
    on.exit(unlink(tmp.dir, TRUE, TRUE))
    config.file <- file.path(tmp.dir, "config.yaml")
    cache <- cachemer$new(path = config.file)
    
    res1 %c-% testFun(1:23, b = 1, list(d = 2, e = 3))
    res2 %c-% testFun(1:23, b = 2, list(d = 2, e = 3))
    res3 %c-% testFun(1:23, b = 3, list(d = 2, e = 3))
    
    restored <- restoreCache(tmp.dir)
    
    testthat::expect_equal(
      names(restored), 
      as.vector(sapply(restored, function(x) x$hash))
    )
  })
  
  testthat::test_that("saveCache: restore session", {
    dir.create(tmp.dir <- tempfile())
    on.exit(unlink(tmp.dir, TRUE, TRUE))
    config.file <- file.path(tmp.dir, "config.yaml")
    cache <- cachemer$new(path = config.file)
    
    res1 %c-% testFun(1:23, b = 1, list(d = 2, e = 3))
    res2 %c-% testFun(1:23, b = 2, list(d = 2, e = 3))
    res3 %c-% testFun(1:23, b = 3, list(d = 2, e = 3))
    
    smry <- cache$summary()
    
    cache$clear()
    
    testthat::expect_length(list.files(tmp.dir), 4)
    testthat::expect_null(cache$lastCache)
      
    # restore session
    cache <- cachemer$new(path = config.file)
    
    testthat::expect_equal(cache$summary(), smry)
    
  })
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
