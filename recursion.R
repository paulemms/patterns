# examples of recursive function definitions

# R's factorial function uses the gamma function, but we require
# non-negative integer arguments
f <- function(n) {
  stopifnot(n >= 0, round(n) == n) # don't do check on each recursion
  ff <- function(n) {
    if (n > 1) n * Recall(n-1) else 1 # use of Recall allows ff to be changed
  }
  ff(n)
}


f(5)
f(1)
f(0)
try(f(0.5))

# from ?local example - don't see how to do check once using local
g <- local({
  k <- function(y) f(y)
  f <- function(n) {
    if (n > 1) n * k(n-1) else 1 # reference to k in env
  }
})

g(5)
try(g(1.5))
g(0)
try(g(0.5))
try(g(-1))


