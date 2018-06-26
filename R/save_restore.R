

if (FALSE) {
  
  # temp
  # dir.create(tmp.dir <- tempfile())
  # on.exit(unlink(tmp.dir, TRUE, TRUE))
  # cache <- cachemer$new(file.path(tmp.dir, "config.yaml"))
  # cache$clear()
  # for (i in 1:22)
  #   res %c-% testFun(a = 1:22, b = i, c = list(d = i / 2, e = i/3))
  # ============================
  
  library(future)
  
  saveCache <-
    function(x = NULL,
             plan = "multiprocess",
             workers = future::availableCores() - 1,
             promises.env) {
      
    
    # quote regardin to saving cache to file
    qt <- quote({
      fresult %<-% {
        cat("Resolving...")
        Sys.sleep(10)
        cat("DONE")
        
        cat(rnorm(1, 1, 4), "\n", file = "dev/save.log", append = TRUE)
        
      }
    })
    
    fof <- tryCatch({
      future::futureOf(promises.env$fresult)
    }, error = function(e) {
      print("'fresult' does not exists")
      NULL
    })
    
    if (is.null(fof)) {
      # if does not exists then procees and create
      eval(qt, envir = promises.env)
    } else {
      # exists so if not resolved then wait
      fof <- future::futureOf(promises.env$fresult)
      
      if (!future::resolved(fof)) {
        future::resolve(fof)
        
      } else {
        eval(qt, envir = promises.env)
      }
      
    }
    
  }
  promises.env <- new.env()
  debugonce(saveCache)
  saveCache(promises.env = promises.env)
  
}
