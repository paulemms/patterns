# Implementation of the iterator (see generator) pattern
rm(list = ls())

# define a generic iterator

interator <- function(inventory_collection, ...) UseMethod("iterator")
interator.default <- function(inventory_collection, ...) {
  c(
    has_next_inventory_item = function() FALSE,
    get_next_inventory_item = function() NULL,
    has_previous_inventory_item = function() FALSE,
    get_previous_inventory_item = function() NULL
  )
}

iterator.numeric <- function(inventory_collection, ...) {
  pos <- 0
  c(
    has_next_inventory_item = function() pos < length(inventory_collection),
    get_next_inventory_item = function() {
      pos <<- pos + 1
      if (pos <= length(inventory_collection)) inventory_collection[[pos]] else NULL
    },
    has_previous_inventory_item = function() pos > length(inventory_collection),
    get_previous_inventory_item = function() {
      pos <<- pos - 1
      if (pos > 0) inventory_collection[[pos]] else NULL
    }
  )
}

iterator.character <- function(inventory_collection, ...) {
  iterator.numeric(inventory_collection)
}

iterator.list <- function(inventory_collection, ...) {
  iterator.numeric(inventory_collection)
}

# key-value pairs
iterator.environment <- function(inventory_collection, type = "key", ...) {
  if (type == "key") {
    iterator.character(names(inventory_collection))
  } else if (type == "value") {
    iterator.list(mget(objects(e, sorted = FALSE), envir = e))
  } else stop()

}

i <- interator(1:10)
i$has_next_inventory_item()
for (j in 1:20) if (i$has_next_inventory_item())
  print(i$get_next_inventory_item()) else print(i$get_previous_inventory_item())


i <- interator(as.character(1:10))
for (j in 1:20) if (i$has_next_inventory_item())
  print(i$get_next_inventory_item()) else print(i$get_previous_inventory_item())

e <- new.env()
e$b <- "2"
e$a <- 1
e$c <- "fish"
i1 <- interator(e)
i2 <- interator(e, type = "value")
for (j in 1:20) if (i1$has_next_inventory_item())
  print(c(i1$get_next_inventory_item(), i2$get_next_inventory_item())) else
    print(c(i1$get_previous_inventory_item(), i2$get_previous_inventory_item()))

