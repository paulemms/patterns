# Implementation of the command pattern

rm(list = ls())

Calculator <- setRefClass(
  "Calculator",
  fields = list(result = "numeric"),
  methods = list(
    initialize = function(result = 0) result <<- result
  )
)


# add command
add <- function(calculator, x) {
  structure(
    list(
      doit = function()
        tryCatch({calculator$result <- calculator$result + x; TRUE},
                 error = function(e) FALSE),
      undoit = function()
        tryCatch({calculator$result <- calculator$result - x; TRUE},
                 error = function(e) FALSE)
    ),
    class = "Command"
  )
}

# double command
dbl <- function(calculator) {
  browser()
  structure(
    list(
      doit = function()
        tryCatch({calculator$result <- 2 * calculator$result; print(traceback()); TRUE},
                 error = function(e) FALSE),
      undoit = function()
        tryCatch({calculator$result <- 0.5 * calculator$result; TRUE},
                 error = function(e) FALSE)
    ),
  class = "Command"
  )
}

# pass the command manager by reference
# see example in setRefClass
CommandManager <- setRefClass("CommandManager",
  fields = list(
    max_history_length = "numeric",
    history_list = "list",
    redo_list = "list"
  ),
  methods = list(
    initialize = function(max_history_length = 100) {
      max_history_length <<- max_history_length
      history_list <<- list()
      redo_list <<- list()
    }
  )
)


invoke_command <- function(command, manager) {

  if (command$doit()) {
    # add to history
    manager$history_list <- c(list(command), manager$history_list)
    if (length(manager$history_list) > manager$max_history_length)
      manager$history_list <- manager$history_list[-length(manager$history_list)]
  } else {
    manager$history_list <- list()
  }
  # ensure redo list empty
  if (length(manager$redo_list) > 0) manager$redo_list <- list()

  return(invisible(manager))
}


invoke_undo <- function(manager) {

  if (length(manager$history_list) > 0) {
    undo_command <- manager$history_list[[1]]
    undo_command$undoit()
    manager$history_list <- manager$history_list[-1]
    manager$redo_list <- c(list(undo_command), manager$redo_list)
  }

  return(invisible(manager))
}

invoke_redo <- function(manager) {

  if (length(manager$redo_list) > 0) {
    redo_command <- manager$redo_list[[1]]
    redo_command$doit()
    manager$redo_list <- manager$redo_list[-1]
    manager$history_list <- c(list(redo_command), list(manager$history_list))
  }

  return(invisible(manager))
}


# Example
manager <- CommandManager$new()
calculator <- Calculator$new()

com_list <- list()
com_list[[1]] <- add(calculator, pi)
com_list[[2]] <- dbl(calculator)
com_list[[3]] <- add(calculator, 10)

# apply all commands
lapply(com_list, invoke_command, manager = manager)
print(manager$history_list)
print(calculator$result)

# undo commands
invoke_undo(manager)
print(calculator$result)
invoke_undo(manager)
print(calculator$result)
invoke_redo(manager)
print(calculator$result)
