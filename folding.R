# examples of folding in R - unfolds?
# see https://en.wikipedia.org/wiki/Fold_(higher-order_function)

# base R - use Reduce and examples
?Reduce
Reduce(`||`, c(F, T, F))
# reversing a list, but use rev
snoc <- function(x, y) c(y, x)
Reduce(snoc, 1:10, init = list())

# sum over tree
# f <- function(x, y) {
#   message('.')
#   sprintf('x:%s y%s', x, y)
#   x[[1]] + y[[1]]
# }
# z <- list(1, list(2, list(3, list(4))))
# Reduce(f, z, 0)

# future.apply apparently has a faster version, but it is not exported
library(future.apply)
future.apply:::fold

