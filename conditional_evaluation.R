# The functions if and ifelse are different functions in R and have a different
# purpose.

# The former works on a logical scalar test expression, while the latter is
# designed to test a logical *vector* expression. For each element of the
# logical vector, ifelse returns the corresponding elements in the then or else
# expressions which are usually vectors. Thus ifelse is a vectorised if
# function. This can lead to surprising results because of R's loose typing, the
# attribute system, and the ambiguities of vectorisation.

# The shape and attributes of the test vector give the shape and attributes of the output
# for ifelse.

# If you have a scalar test you probably mean to use use if
if (TRUE) 2:4 else 5:10
# Note the different size output here
if (FALSE) 2:4 else 5:10
# throws an error if test is NA, so you may need to test for NA with is.na to make sure your code doesn't crash
try(if (NA) 1 else 2)
# Don't pass conditional vectors to if
try(if (c(FALSE, TRUE)) 2:4 else 5:10)


# For ifelse the output structure is the same whatever the condition
ifelse(TRUE, 2:4, 5:10) # works element-wise on the test so returns first number of then
ifelse(FALSE, 2:4, 5:10) # works element-wise on the test so returns first number of else

# With a vector test we get vector output
ifelse(c(FALSE, TRUE), 2:4, 5:7) # first element of else, second element of then
ifelse(c(FALSE, TRUE), 2, 1) # then and else expressions are tiled to match length of test vector
ifelse(c(NA, TRUE), 2:4, 5:7) # handles NA test element and returns NA component

# ifelse can work with objects like list too, but it can be confusing because
# they are converted to a logical vector internally.
ifelse(c(a=FALSE, b=TRUE), list(a=1, b=2), list(a=3, b=4)) # attributes of result are those of test
ifelse(c(b=FALSE, a=TRUE), list(a=1, b=2), list(a=3, b=4)) # names of test are irrelevant for selection, order elements of test matter

# test coerced to logical vector so loses attributes
as.logical(list(a=FALSE, b=TRUE))
ifelse(list(a=FALSE, b=TRUE), list(a=1, b=2), list(a=3, b=4))

# ifelse evaluates the entire then and else clauses and then picks elements
# from them according to the conditional test.

# Next we implement a vectorised ifelse without needless and/or unwanted function
# evaluation of all elements of the then and else expressions.


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
  # Create a result vector with attributes preserved
  result <- x
  result[] <- NA
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

  # Create a result vector with attributes preserved
  result_vec <- cond_vec
  result_vec[] <- NA

  then_vars <- all.vars(substitute(then_expr))
  l <- mget(then_vars, envir = parent.frame())
  # variables whose length is not len are tiled
  l <- lapply(l, \(x) rep(x, length.out=len)[i])
  then_env <- list2env(l, parent = parent.frame())

  else_vars <- all.vars(substitute(else_expr))
  l <- mget(else_vars, envir = parent.frame())
  l <- lapply(l, \(x) rep(x, length.out=len)[j])
  else_env <- list2env(l, parent = parent.frame())

  if (length(i) > 0L) result_vec[i] <- eval(substitute(then_expr), envir=then_env)
  if (length(j) > 0L) result_vec[j] <- eval(substitute(else_expr), envir=else_env)

  result_vec
}

x <- 1:6
a <- -1:-3
b <- 200
ifelse(c(z = x > length(x) / 2 + a), x^3+a, x^2+b)
if_else2(c(z = x > length(x) / 2 + a), x^3+a, x^2+b)

# Pass in expressions rather than functions
if_else2 <- function(cond_vec, then_expr, else_expr) {
  stopifnot(is.logical(cond_vec))
  i <- which(cond_vec)
  j <- which(!cond_vec)
  len <- length(cond_vec)

  # Create a result vector with attributes preserved
  result_vec <- cond_vec
  result_vec[] <- NA

  then_vars <- all.vars(substitute(then_expr))
  l <- mget(then_vars, envir = parent.frame())
  # variables whose length is not len are tiled
  l <- lapply(l, \(x) rep(x, length.out=len)[i])
  then_env <- list2env(l, parent = parent.frame())

  else_vars <- all.vars(substitute(else_expr))
  l <- mget(else_vars, envir = parent.frame())
  l <- lapply(l, \(x) rep(x, length.out=len)[j])
  else_env <- list2env(l, parent = parent.frame())

  if (length(i) > 0L) result_vec[i] <- eval(substitute(then_expr), envir=then_env)
  if (length(j) > 0L) result_vec[j] <- eval(substitute(else_expr), envir=else_env)

  result_vec
}
# This shows that if you have a conditional ifelse( x %% 2 == 0, f1(x), f2(x) )
# then f1 gets called on odd elements of x , and f2 gets called on even
# elements, even though the form of the conditional makes it look as though they
# don't. If f1 isn't designed for odds, or f2 for evens, this could cause
# problems.
#
hate_odds <- function(x) {
  if (any(x %% 2 == 1)) warning(sprintf("I hate odd numbers (%s). ",
                                     paste(x[x%% 2 == 1], collapse = ", ")))
  x
}

hate_evens <- function(x) {
  if (any(x %% 2 == 0)) warning(sprintf("I hate even numbers (%s). ",
                                     paste(x[x%% 2 == 0], collapse = ", ")))
  x
}
test_that("Partial evaluation works", {
  x <- 1:100
  expect_warning(ifelse(x %% 2 == 1, hate_odds(x) / 2, hate_evens(x) + 3))
  expect_warning(if_else(x %% 2 == 0, hate_odds(x) / 2, hate_evens(x) + 3))
  expect_no_warning(if_else2(x %% 2 == 0, hate_odds(x) / 2, hate_evens(x) + 3))
})

# TODO: add more test_that cases

# Our if_else2 may give different results to the built in ifelse.
# For example, it is ambiguous what is wanted if the mean function is
# included in the then or else clauses because it depends on the length of the
# vector passed.

# Different output because the subsetted vector is passed to the clauses in
# if_else2
ifelse(x > length(x) / 2 + a, x - mean(x), x - median(x))
if_else2(x > length(x) / 2 + a, x - mean(x), x - median(x))

# In practice, it might be better to keep evaluation on the entire vector
# and set elements to NA if they are not to be evaled. This requires
# functions to return NA on those elements and not throw errors or warnings.
# Whether this is practical depends on the application.

if_else3 <- function(cond_vec, then_expr, else_expr) {
  stopifnot(is.logical(cond_vec))

  i <- which(cond_vec)
  j <- which(!cond_vec)
  len <- length(cond_vec)

  # Create a result vector with attributes preserved
  result_vec <- cond_vec
  result_vec[] <- NA

  then_vars <- all.vars(substitute(then_expr))
  l <- mget(then_vars, envir = parent.frame())
  # variables whose length is not len are tiled
  l <- lapply(l, \(x) {
    tmp_then <- rep(NA, len); tmp_then[i] <- rep(x, length.out=len)[i]; tmp_then
    })
  then_env <- list2env(l, parent = parent.frame())

  else_vars <- all.vars(substitute(else_expr))
  l <- mget(else_vars, envir = parent.frame())
  l <- lapply(l, \(x) {
    tmp_else <- rep(NA, len); tmp_else[j] <- rep(x, length.out=len)[j]; tmp_else
    })
  else_env <- list2env(l, parent = parent.frame())

  if (length(i) > 0L) result_vec[i] <- eval(substitute(then_expr), envir=then_env)[i]
  if (length(j) > 0L) result_vec[j] <- eval(substitute(else_expr), envir=else_env)[j]

  result_vec
}

# Example
z <- c(-2, 2)
ifelse(z > 0, factorial(z), exp(z))
if_else2(z > 0, factorial(z), exp(z))
if_else3(z > 0, factorial(z), exp(z)) # no warning with conditional eval

# But it is conceivable that we might need to evaluate the subsetted and the full variable
# in the then expression e.g.
ifelse(x > length(x) / 2 + a, x - mean(x), x - median(x))
if_else2(x > length(x) / 2 + a, x - mean(x), x - median(x))
# BUT now we need na.rm = TRUE because we have NAs in x
if_else3(x > length(x) / 2 + a, x - mean(x, na.rm = TRUE), x - median(x, na.rm = TRUE))

# We can force evaluation on a full vector by passing in extra variables
if_else4 <- function(cond_vec, then_expr, else_expr, ...) {
  stopifnot(is.logical(cond_vec))
  add_vars <- list(...)
  i <- which(cond_vec)
  j <- which(!cond_vec)
  len <- length(cond_vec)

  # Create a result vector with attributes preserved
  result_vec <- cond_vec
  result_vec[] <- NA

  then_vars <- setdiff(all.vars(substitute(then_expr)), names(add_vars))
  l <- mget(then_vars, envir = parent.frame())
  # variables whose length is not len are tiled
  l <- lapply(l, \(x) rep(x, length.out=len)[i])
  then_env <- list2env(c(l, add_vars), parent = parent.frame())

  else_vars <- setdiff(all.vars(substitute(else_expr)), names(add_vars))
  l <- mget(else_vars, envir = parent.frame())
  l <- lapply(l, \(x) rep(x, length.out=len)[j])
  else_env <- list2env(c(l, add_vars), parent = parent.frame())

  if (length(i) > 0L) result_vec[i] <- eval(substitute(then_expr), envir=then_env)
  if (length(j) > 0L) result_vec[j] <- eval(substitute(else_expr), envir=else_env)

  result_vec
}

if_else4(x > length(x) / 2 + a, x - y1, x - y2, y1 = mean(x), y2 = median(x))
