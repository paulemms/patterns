# Implementation of the filter pattern based on turning base::Filter into a generic
rm(list = ls())

# The Filter examples ...

## A list of all functions in the base environment:
funs <- Filter(is.function, sapply(ls(baseenv()), get, baseenv()))
## Functions in base with more than 10 arguments:
names(Filter(function(f) length(formals(f)) > 10, funs))
## Number of functions in base with a '...' argument:
length(Filter(function(f)
  any(names(formals(f)) %in% "..."),
  funs))

## Find all objects in the base environment which are *not* functions:
Filter(Negate(is.function),  sapply(ls(baseenv()), get, baseenv()))


# Filter example using a closure to give second argument
file_contains <- function(s) {
  function(y) grepl(s, lapply(y, function(x) paste(readLines(x), collapse = "\n")))
}

Filter(file_contains("manager"), dir("."))

# Filter can work on lists so we can filter objects
obj <- function(x) structure(list(name = x), class = "Z")

Filter(\(x) x$name == "B", list(obj("A"), obj("B"), obj("C")))


# Turn Filter into generic ??? but not sure if there is an application?
Filter <- function(f, x, ...) UseMethod("Filter")
Filter.default <- function(f, x, ...) base::Filter(f, x, ...)

# dispatch is on first argument which is function so create functions that
# subclass function ??

