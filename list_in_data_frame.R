# Each column of a data.frame must have the same type, but
# it does not need to atomic. Here, we show an example
# of a column with type 'list'.
rm(list = ls())
library(tidyverse)

tbl <- tibble(
  ID =  c(1,1,1,1,2,2,3,3,3,3),
  Day = c(1,2,4,7,2,3,1,3,4,8),
  Pain = c(10,9,7,2,8,7,10,6,6,2)
)

z <- c(1,4)
#z <- c(2, 7)

a <- aggregate(tbl["Day"], tbl["ID"], identity)

tbl %>%
  group_by(ID) %>%
  summarise(Days = list(Day)) %>%
  filter(map_lgl(Days, ~ all(z %in% .x))) %>%
  as.data.frame()

# without summarise and retaining Pain data
tbl %>%
  group_by(ID) %>%
  filter(map_lgl(list(Day), ~ all(z %in% .x)), Day %in% z) %>%
  as.data.frame()
