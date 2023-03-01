# Implementation of the decorator pattern using generics
rm(list = ls())

simple_coffee <- function(name) {
  structure(list(name = name, ingredients = "Coffee", cost = 1), class = "Coffee")
}

# decorators
with_milk <- function(coffee) {
  structure(list(name = coffee$name, decorated_coffee = coffee, ingredients = "Milk", cost = 0.5),
            class = c("MilkCoffee", class(coffee)))
}

with_sprinkles <- function(coffee) {
  structure(list(name = coffee$name, decorated_coffee = coffee, ingredients = "Sprinkles", cost = 0.2),
            class = c("CoffeeWithSprinkles", class(coffee)))
}

cost <- function(coffee, ...) UseMethod("cost")
cost.default <- function(coffee, ...) 0

cost.Coffee <- function(coffee, ...) coffee$cost
cost.MilkCoffee <- function(coffee, ...) {
  coffee$cost + NextMethod(coffee = coffee$decorated_coffee)
}
cost.CoffeeWithSprinkles <- function(coffee, ...) {
  coffee$cost + NextMethod(coffee = coffee$decorated_coffee)
}

ingredients <- function(coffee, ...) UseMethod("ingredients")
ingredients.default <- function(coffee, ...) 0

ingredients.Coffee <- function(coffee, ...) coffee$ingredients
ingredients.MilkCoffee <- function(coffee, ...) {
  c(coffee$ingredients, NextMethod(coffee = coffee$decorated_coffee))
}
ingredients.CoffeeWithSprinkles <- function(coffee, ...) {
  c(coffee$ingredients, NextMethod(coffee = coffee$decorated_coffee))
}

coff <- simple_coffee("a")
ingredients(coff)
cost(coff)

mc <- with_milk(coff)
ingredients(mc)
cost(mc)

mcs <- with_sprinkles(mc)
ingredients(mcs)
cost(mcs)
