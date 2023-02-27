
#' @examples
#' a <- R6::R6Class("a", public = list(df = NULL, initialize = function() self$df <- iris))
#' x <- get_instance(a)
#' x$df
#' x <- get_instance(a)
get_instance <- local({
  inst <- NULL

  function(obj)
    if (is.null(inst)) {
      message('New object')
      inst <<- obj$new()
    } else inst
})


#' Instance of fixed class
#' @examples
#' a <- R6::R6Class("a", public = list(df = NULL, initialize = function() self$df <- iris))
#' get_instance2 <- get_builder(a)
#' x <- get_instance2()
#' x$df
#' x <- get_instance2()
get_builder <- function(cl) {
  inst <- NULL

  function()
    if (is.null(inst)) {
      message('New object')
      inst <<- cl$new()
    } else inst
}


Singleton <- R6::R6Class(
  classname = "Singleton",
  private = list()
)
