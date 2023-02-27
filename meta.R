# create functions from https://stackoverflow.com/questions/12982528/how-to-create-an-r-function-programmatically
make_function1 <- function(args, body, env = parent.frame()) {
  args <- as.pairlist(args)
  eval(call("function", args, body), env)
}
make_function2 <- function(args, body, env = parent.frame()) {
  f <- function() {}
  formals(f) <- args
  body(f) <- body
  environment(f) <- env

  f
}
make_function3 <- function(args, body, env = parent.frame()) {
  as.function(c(args, body), env)
}
make_function4 <- function(args, body, env = parent.frame()) {
  subs <- list(args = as.pairlist(args), body = body)
  eval(substitute(`function`(args, body), subs), env)
}
