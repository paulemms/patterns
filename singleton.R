# Various ways to ensure a single instance of an object is created

#' Instance of any class
#'
#' On first call of get_instance object is created, thereafter call yield this object
#' @examples
#' a <- R6::R6Class("a", public = list(df = NULL, initialize = function() self$df <- iris))
#' x <- get_instance(a)
#' x$df
#' x <- get_instance(a)
get_instance <- local({
  inst <- NULL

  function(cl)
    if (is.null(inst)) {
      message('New object')
      inst <<- cl$new()
    } else inst
})


#' Instance of fixed class
#'
#' @examples
#' a <- R6::R6Class("a", public = list(df = NULL, initialize = function() self$df <- iris))
#' get_instance_a <- get_builder(a)
#' x <- get_instance_a()
#' x$df
#' x <- get_instance_a()
get_builder <- function(cl) {
  inst <- NULL

  function()
    if (is.null(inst)) {
      message('New object')
      inst <<- cl$new()
    } else inst
}


