# Implementation of ifelse without needless function evaluation
library(dplyr)
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

# Jocelyn Ireson-Paine example as function and handle NAs
my_if_else <- function(x, cond, f1, f2) {
  i <- which(cond(x))
  j <- which(!cond(x))
  result <- rep(NA, length(x))
  result[i] <- f1(x[i])
  result[j] <- f2(x[j])

  result
}

# ifelse on different types
new_conditional <- function(x, ...) {
  UseMethod("new_conditional", x)
}

new_conditional.numeric <- function(v, mask) {
  structure(list(v = v, mask = mask), class = "ConditionalVector")
}

new_conditional.data.frame <- function(df, mask) {
  structure(list(df = df, mask = mask), class = "ConditionalDataFrame")
}

gif_else <- function(x, ...) {
  UseMethod("gif_else", x)
}


gif_else.ConditionalVector <- function(cv, FUN_IF = f, FUN_ELSE = g) {

  result <- rep(NA, length(cv$v))
  i <- which(cv$mask)
  j <- which(!cv$mask)
  result[i] <- FUN_IF(cv$v[i])
  result[j] <- FUN_ELSE(cv$v[j])

  result
}

gif_else.ConditionalDataFrame <- function(cdf, FUN_IF = f, FUN_ELSE = g, ...) {

  result <-cdf$mask
  result[] <- NA
  i <- which(cdf$mask)
  j <- which(!cdf$mask)
  result[i] <- FUN_IF(cdf$df[i,,drop=FALSE], ...)
  result[j] <- FUN_ELSE(cdf$df[j,,drop=FALSE], ...)

  result
}

my_if_else(x, cond, cube, square)
if_else(cond(x), cube(x), square(x))
cv <- new_conditional(x, cond(x))
gif_else(cv, cube, square)

cond2 <- function(df) with(df, 2 * x == y)
add <- function(df, z) with(df, x + y) + z
mult <- function(df, z) with(df, x * y) * z
df <- data.frame(x = c(1, 2), y = c(3, 4))

cdf <- new_conditional(df, cond2(df))
gif_else(cdf, add, mult, z = 1)

