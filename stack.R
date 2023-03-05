# functional stack implementations
rm(list = ls())

# pop and push access the stack through their enclosed environment
stack1 <- local({
  s <- list()
  c(
    pop = function() { if ((n = length(s)) > 0) {x <- s[[n]]; s <<- s[-n]; x} else NULL},
    push = function(x) s[[length(s) + 1]] <<- x
  )
})

s <- stack1
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

# make stacks that return the push function - initial_stack gives type and contents of stack
new_stack <- function(initial_stack) {
  stack <- initial_stack
  # recursion function keeps the enclosing environment available from push
  push_fun <- function(item) {stack <<- append(item, stack); invisible(push_fun)}
  invisible(push_fun)
}
pop <- function(push_fun) {
  item <- environment(push_fun)$stack[1]
  evalq(stack <- stack[-1], envir = environment(push_fun))
  item
}
content <- function(push_fun) environment(push_fun)$stack

push <- new_stack(list("a", "b")) # new_stack returns a push function
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
