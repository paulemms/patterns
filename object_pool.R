# Implementation of the object pool pattern
rm(list = ls())

# example of a limited cache of file contents

file_cache <- function(pool_size = 3) {
  pool <- new.env(parent = emptyenv())
  list(
    get_file = function(f) {
      if (f %in% objects(pool)) {
        message("Retrieved file from cache")
        pool[[f]]
      } else {
        contents <- readLines(f)
        message("Read file")
        if (length(objects(pool)) < pool_size) {
          message("Cached file")
          pool[[as.character(f)]] <- contents
        }
        contents
      }
    },
    clear = function() rm(list = objects(pool), envir = pool)
  )
}

fc <- file_cache()
head(fc$get_file("command.R"))
head(fc$get_file("curry.R"))
head(fc$get_file("factory_method.R"))
head(fc$get_file("command.R"))
head(fc$get_file("immutable.R"))
head(fc$get_file("immutable.R"))
fc$clear()
head(fc$get_file("command.R"))
