# Implementation of the immutable pattern
rm(list = ls())

# use a closure
immutable <- function(start_value) {
  value <- start_value
  # other objects ...

  lockEnvironment(environment(), bindings = TRUE) # every binding in the function is locked
  function() value
}

# example
get_value <- immutable(10)
get_value()

# cannot change value using environment where get_value defined
try(environment(get_value)$value <- 11)
get_value()

# Although you can just unlock the binding
unlockBinding("value", environment(get_value))
environment(get_value)$value <- 11
get_value()
