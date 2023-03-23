# stack implementations
rm(list = ls())

# functional stack - repeated crerates copies of existing stack
new_stack <- function(items = list()) {
  structure(list(items = items), class = "stack")
}
push <- function(s, y) {
  s$items <- c(list(y), s$items)
  s
}
pop <- function(s) {
  s$items <- s$items[-1]
  s
}
# extra function needed since pop must return stack
top <- function(s) head(s$items, 1)

s <- new_stack(1:4)
s <- push(s, 10) # need to reassign to maintain stack state - new stack created
top(s)
s <- push(s, 20)
s <- pop(s) # need to reassign to maintain stack state
top(s)
s <- push(s, 30)
top(s)
s

# pop and push access the stack through their enclosed environment - just a primitive class really
stack1 <- local({
  s <- list()
  c(
    pop = function() { if ((n = length(s)) > 0) {x <- s[[n]]; s <<- s[-n]; x} else NULL},
    push = function(x) s[[length(s) + 1]] <<- x
  )
})

s <- stack1
s$push(10) # modifies s so not functional
s$push(20)
s$pop()

# Reference class implementation
Stack <- setRefClass(
  "Stack",
  fields = list(items = "list"),
  methods = list(
    push = function(x) {
      items <<- c(items, x)
      invisible(.self)
    },
    pop = function() {
      if ((n = .self$length()) > 0) {
        item <- items[[n]]
        items <<- items[-n]
        item # just returns the item - no need for top
      } else NULL
    },
    length = function() base::length(items)

  )
)
s <- Stack$new()
s$push(10)
s$push(20)
s$pop()

# use a list in an environment for the stack which is passed by reference
stack2 <- structure({e <- new.env(); e$s <- list(); e}, name = "Stack")
push <- function(st, x) st$s <- append(x, st$s)
pop <- function(st, x) {y <- st$s[1]; st$s <- st$s[-1]; y}

push(stack2, 10)
push(stack2, 20)
push(stack2, "fish")
pop(stack2)
pop(stack2)
pop(stack2)

# make stacks that return the push function
make_stack <- function(initial_stack) {
  stack <- new_stack(initial_stack)
  # recursion function keeps the enclosing environment available from push
  push_fun <- function(item) {stack$items <<- append(item, stack$items); invisible(push_fun)}
  invisible(push_fun)
}

pop <- function(push_fun) {
  item <- environment(push_fun)$stack$items[1]
  evalq(stack$items <- stack$items[-1], envir = environment(push_fun))
  item
}
content <- function(push_fun) environment(push_fun)$stack$items

push <- make_stack(list("a", "b")) # new_stack returns a push function
content(push)
push(10) # enclosing environment of push gives access to stack
push(20)
push(21)(22) # multiple pushes
s5 <- push(23) # assignment gives pop access to stack in push
pop(s5) # returns top of stack
push(list("30", 40, 50, 60, 70)) # single push leaves 30 on top
content(s5)
pop(s5)
pop(s5);pop(s5) # two pops
content(s5)


# create and manipulate a linked list stack

# for last element of linked list
end_linked_list <- structure(0, class = 'EndLinkedList')

# nested list - use first element pointer then value to make
# top cheaper?
x <- list(1, list(\(x) x, end_linked_list))

# flatten linked list - see rlang too
fl <- \(z) { l <- as.list(unlist(z)); l[-length(l)] }

# push
x1 <- rapply(x, \(y) list(3, end_linked_list),
             classes = 'EndLinkedList', how = 'replace')
fl(x1)

# remove last element - pop
x2 <- rapply(x, \(y) end_linked_list,
             classes = 'EndLinkedList', how = 'replace')
fl(x2)

# top
tail(fl(x2), 1)

# use a replacement functions for stack

`push<-` <- function(x, value) {
  c(x, value)
}

`pop<-` <- function(x, value) {
  x[-1]
}

x <- 1:5
push(x) <- 6:10
(pop(x) <- x[1])
