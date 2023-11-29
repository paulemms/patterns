# examples of Monads in R
# function factory and composition
# see https://www.brodrigues.co/blog/2022-04-11-monads/
# https://adv-r.hadley.nz/function-factories.html
# https://en.wikipedia.org/wiki/Monad_(functional_programming)
# https://wiki.haskell.org/All_About_Monads
# https://blog.ploeh.dk/2022/03/28/monads/

data(starwars)

# base R with pipe
starwars |>
  aggregate(height ~ species == 'Human', FUN = mean) |>
  setNames(c('is_human', 'mean_height'))

# tidyverse
starwars %>%
  drop_na(species) %>%
  group_by(is_human = species == "Human") %>%
  summarise(mean_height = mean(height, na.rm = TRUE)) %>%
  as.data.frame()

# timing
tic <- Sys.time()
starwars %>%
  filter(skin_color == "light") %>%
  select(species, sex, mass) %>%
  group_by(sex, species) %>%
  summarise(
    total_individuals = n(),
    min_mass = min(mass, na.rm = TRUE),
    mean_mass = mean(mass, na.rm = TRUE),
    sd_mass = sd(mass, na.rm = TRUE),
    max_mass = max(mass, na.rm = TRUE)
  ) %>%
  select(-species) %>%
  tidyr::pivot_longer(-sex, names_to = "statistic", values_to = "value")
toc <- Sys.time()
(running_time <- toc - tic)

# A monad is a function factory and a flattening function for composition

# function factory - build a function and return it
timeit <- function(.f, ..., running_time = 0){

  function(..., .running_time = running_time){

    tic <- Sys.time()

    result <- .f(...)

    toc <- Sys.time()

    list(result = result,
         running_time = toc - tic + .running_time) # monadic value
  }

}

# we can time any function by building it enclosed with timing
t_sqrt <- timeit(sqrt)
t_sqrt(10)
t_log <- timeit(log)
t_log(10)

# compose using the flattening function
bind <- function(.l, .f, ...){

  .f(.l$result, ..., .running_time = .l$running_time)

}

# cumulative running time of composite function
10 |>
  t_sqrt() |>
  bind(t_log)

