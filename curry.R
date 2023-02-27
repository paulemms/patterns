# Partial application and currying
# see https://stackoverflow.com/questions/2228544/higher-level-functions-in-r-is-there-an-official-compose-operator-or-curry-fun
# and the functional package
rm(list=ls())

# from functional, but this is partial application not currying
PA <- function(FUN, ...) {
  .orig = list(...)
  function(...) do.call(FUN, c(.orig, list(...)))
}

# Examples
double <- PA(`*`, e1=2)
double(4)

f1 <- function(x) x
f2 <- function(x, y) x + y
f3 <- function(x, y, z) x + y + z
f4 <- function(x, y, z, a) x + y + z + a
f5 <- function(x, y, z) x*y*z
pa <- PA(f3, 1)
try(pa(2)) # should produce another function

Curry <- function(FUN) {
  stopifnot(!is.primitive(FUN))
  # inner recursive function for composition
  inner <- function(fm) {
    call("function", as.pairlist(fm[1]),
         if (length(fm) <= 1) body(FUN) else inner(fm[-1]))
  }
  eval(inner(formals(FUN)))
}

# Examples
Curry(function() {})
curried_f4 <- Curry(f4)
curried_f4(6)
curried_f5 <- Curry(f5)
curried_f5(6) # single arg function
curried_f5(6)(2) # single arg function
curried_f5(6)(2)(3)

# primitive version - seems to use different argument matching
Curryp <- function(FUN) {
  fm <- formals(args(FUN)) # need args for primitive
  # cannot use body for primitive functions
  bdy <- if (is.primitive(FUN)) as.call(c(FUN, lapply(names(fm), str2lang))) else body(FUN)
  # inner recursive function for composition
  inner <- function(fm) {
    call("function", as.pairlist(fm[1]),
         if (length(fm) <= 1) bdy else inner(fm[-1]))
  }
  eval(inner(fm))
}

z <- Curryp(`c`)
z(2, 3)
z <- Curryp(`+`)
z(3)(4)

