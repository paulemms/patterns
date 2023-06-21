# calculations on infinite lists
# https://nus-cs2030s.github.io/2223-s2/33-infinitelist.html
rm(list = ls())

recursive_list <- function(hd, tl = NULL) structure(c(list(hd), tl), class = "RecursiveList")

gen_list <- function(fun, size = Inf) {
  recursive_list(fun(), if (size > 1) call("gen_list", fun, size - 1))
}

cond_list <- function(start, iterate_fun, cond_fun = \(x) TRUE) {
  if (cond_fun(start))
    recursive_list(start,
      call("cond_list", iterate_fun(start), iterate_fun, cond_fun))
  else NULL
}

as_list <- function(x, ...) {
  c(list(head(x)), if (is.null(tail(x))) NULL else as_list(tail(x)))
}

# public <R> EagerList<R> map(Transformer<? super T, ? extends R> mapper) {
#   return new EagerList<>(mapper.transform(this.head()), this.tail().map(mapper));
# }
# We can also provide the filter method, that takes in lambda expression as a parameter and tests if each element in the list passes the test. We return the list containing only the elements that pass the given test.
#
#
# public EagerList<T> filter(BooleanCondition<? super T> cond) {
#   if (cond.test(this.head())) {
#     return new EagerList<>(this.head(), this.tail().filter(cond));
#   }
#   return this.tail().filter(cond);
# }

map_list <- function(x, fun) {
  recursive_list(fun(head(x)), call("map_list", tail(x), fun))
}

# adds NULLs for unfiltered atm
filter_list <- function(x, fun) {
  if (is.null(x)) return(NULL)
  hd <- head(x)
  tl <- tail(x)
  if (fun(hd)) {
    recursive_list(hd, call("filter_list", tl, fun))
  } else {
    recursive_list(NULL, call("filter_list", tl, fun))
  }
}

head.RecursiveList <- function(x, ...) {
  x[[1]]
}

tail.RecursiveList <- function(x, ...) {
  if (length(x) > 1) eval(x[[2]]) else NULL
}

get_element <- function(x, n) {
  if (n == 1) head(x) else get_element(tail(x), n - 1)
}

# EagerList<Integer> l = EagerList.iterate(1, i -> i < 10, i -> i + 1) // [1, ..., 9]
# .filter(i -> i % 3 == 0)  // [3, 6, 9]
# .map(i -> i * 2);  // [6, 12, 18]
# l.head();        // 6
# l.tail().head(); // 12
# l.tail().tail().head(); // 18
# l.get(2);               // 18
by_1 <- cond_list(1, \(x) x + 1, \(x) x < 10)
as_list(by_1)
filtered_by_1 <- filter_list(by_1, \(x) x %% 3 == 0)
as_list(filtered_by_1)
head(by_1)
head(tail(by_1))
head(tail(tail(by_1)))
get_element(by_1, 5)

ones <- gen_list(\(x) 1, 4)
as_list(ones)
head(ones)
get_element(ones, 3)

# InfiniteList<Integer> ones = InfiniteList.generate(() -> 1); // 1, 1, 1, 1, ....
# InfiniteList<Integer> evens = InfiniteList.iterate(0, x -> x + 2); // 0, 2, 4, 6, ...
# evens.head(); // -> 0
# evens.get(5); // -> 10
# evens = evens.tail();
# evens.head(); // -> 2
# evens.get(6); // -> 14

# infinte length lists
ones <- gen_list(\(x) 1)
get_element(ones, 4)
evens <- cond_list(0, \(x) x + 2)
head(evens)
get_element(evens, 4)
tevens <- tail(evens)
head(tevens)
get_element(tevens, 7)
