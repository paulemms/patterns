# Implementation of ifelse without needless and/or unwanted function evaluation
# on different data types using a mask

rm(list = ls())

# test functions
cube <- function(x) x^3
square <- function(x) x^2

# test data
x <- c(NA, 2:5, NA)
cond <- function(x) x > length(x) / 2 + 1
cube(x)
square(x)

# partial evaluation on different types using a mask
new_conditional <- function(x, ...) {
  UseMethod("new_conditional", x)
}

new_conditional.numeric <- function(v, mask) {
  stopifnot(length(mask) == length(v))
  structure(list(v = v, mask = mask), class = "ConditionalVector")
}

# partial evaluation on columns of a data.frame
new_conditional.data.frame <- function(df, mask) {
  stopifnot(length(mask) == nrow(df))
  structure(list(df = df, mask = mask), class = "ConditionalDataFrame")
}

# partial ifelse on different types
pif_else <- function(x, FUN_IF, FUN_ELSE, ...) {
  stopifnot(is.function(FUN_IF), is.function(FUN_ELSE))
  UseMethod("pif_else", x)
}

pif_else.ConditionalVector <- function(cv, FUN_IF, FUN_ELSE, ...) {

  result <- cv$mask
  result[] <- NA
  i <- which(cv$mask)
  j <- which(!cv$mask)
  if (length(i) > 0L) result[i] <- FUN_IF(cv$v[i])
  if (length(j) > 0L) result[j] <- FUN_ELSE(cv$v[j])

  result
}

# partial if else on columns in a data.frame
pif_else.ConditionalDataFrame <- function(cdf, FUN_IF, FUN_ELSE, ...) {

  result <-cdf$mask
  result[] <- NA
  i <- which(cdf$mask)
  j <- which(!cdf$mask)
  if (length(i) > 0L) result[i] <- FUN_IF(cdf$df[i,,drop=FALSE], ...)
  if (length(j) > 0L) result[j] <- FUN_ELSE(cdf$df[j,,drop=FALSE], ...)

  result
}

cv <- new_conditional(x, cond(x))
pif_else(cv, cube, square)

cond2 <- function(df) with(df, 2 * x == y)
add <- function(df, z) with(df, x + y) + z
mult <- function(df, z) with(df, x * y) * z
dfr <- data.frame(x = c(1, 2), y = c(3, 4))

cdf <- new_conditional(dfr, cond2(dfr))
dfr$z <- pif_else(cdf, add, mult, z = 1)
dfr

