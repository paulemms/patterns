# The functions if and ifelse are different functions in R and have a different
# purpose.

# The latter is a vectorised version of if and it can lead to surprising results
# because of R's loose typing, the attribute system, and the ambiguities of vectorisation.

# ifelse is usually given a logical *vector* to test and returns
# the corresponding elements in the then or else expressions.
# The shape and attributes of the test vector give the shape and attributes of the output.
if (TRUE) 2:4 else 5:7 # if you have a scalar test often just use if
ifelse(TRUE, 2:4, 5:7) # works element-wise on the test so returns one number
ifelse(c(FALSE, TRUE), 2:4, 5:7)
ifelse(c(FALSE, TRUE), 2, 1) # then and else expressions are tiled

# ifelse can work with objects like list too, but it can be confusing because they are coverted to a vector internally.
ifelse(c(a=FALSE, b=TRUE), list(a=1, b=2), list(a=3, b=4)) # attributes of result are those of test
ifelse(c(b=FALSE, a=TRUE), list(a=1, b=2), list(a=3, b=4)) # names of test are irrelevant for selection, order elements of test matter
as.logical(list(a=FALSE, b=TRUE))
ifelse(list(a=FALSE, b=TRUE), list(a=1, b=2), list(a=3, b=4)) # test coerced to logical vector so loses attributes

# ifelse evaluates the entire then and else clauses and then picks elements
# from them according to test.

# Next we implement a vectorised ifelse without needless and/or unwanted function
# evaluation of all elements of the then and else expressions.

# Our ifelse may give different results to the builtin ifelse.
# For example, it is ambiguous what is wanted if the mean function is
# included in the then or else expressions because it depends on the length of the
# vector passed.

# In practice, it might be better to keep evaluation on the entire vector
# and set elements to NA if they are not to be evaled. This requires
# functions to return NA on those elements and not throw errors or warnings.
# Whether this is practical depends on the application.


library(testthat)
library(dplyr) # for another if_else
rm(list = ls())

# test functions
cube <- function(x) x^3
square <- function(x) x^2

# test data
x <- c(NA, 2:5, NA)
cond <- function(x) x > length(x) / 2 + 1
cube(x)
square(x)

# built-in R element wise conditional
ifelse(cond(x), cube(x), square(x))

# Take functions rather than expressions and handle NAs
if_else1 <- function(x, cond, f1, f2) {
  b <- cond(x)
  i <- which(b)
  j <- which(!b)
  result <- rep(NA, length(x))
  # only evaluate elements of f1 when b is TRUE
  if (length(i) > 0L) result[i] <- f1(x[i])
  if (length(j) > 0L) result[j] <- f2(x[j])

  result
}

test_that('ifelse and if_else reproduced', {
  expect_equal(if_else1(x, cond, cube, square),
               ifelse(cond(x), cube(x), square(x)))
  expect_equal(if_else1(x, cond, cube, square),
               if_else(cond(x), cube(x), square(x)))
})


# Pass in expressions rather than functions
if_else2 <- function(cond_vec, then_expr, else_expr) {
  stopifnot(is.logical(cond_vec))
  i <- which(cond_vec)
  j <- which(!cond_vec)
  len <- length(cond_vec)

  # Create a result vector as yet undefined.
  result_vec <- rep(NA, len)

  then_vars <- all.vars(substitute(then_expr))
  l <- mget(then_vars, envir = parent.frame())
  # variables whose length is not len are tiled
  l <- lapply(l, \(x) if (length(x) == len) x[i] else rep(x, length.out=len)[i])
  then_env <- list2env(l, parent = parent.frame())

  else_vars <- all.vars(substitute(else_expr))
  l <- mget(else_vars, envir = parent.frame())
  l <- lapply(l, \(x) if (length(x) == len) x[j] else rep(x, length.out=len)[j])
  else_env <- list2env(l, parent = parent.frame())

  if (length(i) > 0L) result_vec[i] <- eval(substitute(then_expr), envir=then_env)
  if (length(j) > 0L) result_vec[j] <- eval(substitute(else_expr), envir=else_env)

  result_vec
}

x <- 1:6
e <- new.env()
a <- -1:-3
b <- 200
ifelse(x > length(x) / 2 + a, x^3+a, x^2+b)
if_else2(x > length(x) / 2 + a, x^3+a, x^2+b)


# TODO: add more test_that cases

# This shows that if you have a
# conditional
#     ifelse( x %% 2 == 0, f1(x), f2(x) )
# then f1 gets called on odd
# elements of x , and f2 gets called
# on even elements, even though
# the form of the conditional makes
# it look as though they don't.
# If f1 isn't designed for odds,
# or f2 for evens, this could cause
# crashes.
#
test_that("Partial evaluation works", {
  hate_odds <- function(x) {
    if (any(x %% 2 == 1)) stop(sprintf("I hate odd numbers (%s). ",
                                       paste(x[x%% 2 == 1], collapse = ", ")))
    x
  }

  hate_evens <- function( x ) {
    if (any(x %% 2 == 0)) stop(sprintf("I hate even numbers (%s). ",
                                       paste(x[x%% 2 == 1], collapse = ", ")))
    x
  }

  x <- 1:100
  expect_error(ifelse(x %% 2 == 0, hate_odds(x) / 2, hate_evens(x) + 3))
  expect_error(if_else(x %% 2 == 0, hate_odds(x) / 2, hate_evens(x) + 3))
  expect_no_error(if_else2(x %% 2 == 0, hate_odds(x) / 2, hate_evens(x) + 3))
})

# different output if aggregate functions included in then or else expressions
# because the subsetted vector is passed to the clauses in if_else2
ifelse(x > length(x) / 2 + a, x - mean(x), x - median(x))
if_else2(x > length(x) / 2 + a, x - mean(x), x - median(x))

# TODO:  select case i.e. nested if_else
# TODO: promises in expressions?

