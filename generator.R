# Pattern to return a generator function in R
# avoids running out of memory for aggregate calculations on large data sets

library(readr)
library(fpeek)

# 4gb file
file_name <- 'c:/data/custom_1988_2020.csv'

peek_head(file_name)
peek_tail(file_name)

# read_csv wants binary connection using open = 'rb, but then chunks corrupted
file_generator <- function(file_name, n = 1000000L, ...) {
  con <- file(file_name, open="r")
  function() read.csv(con, nrows = n, ...)
}

# Find mean of last column with one pass using generator
get_mean <- function(file_names, col) {
  on.exit(close(environment(chunk)$con))

  chunk <- file_generator(file_name, col.names = letters[1:8])
  total <- 0
  num_lines <- 0

  repeat {
    df <- chunk()
    if ((n <- nrow(df)) == 0L)
      break
    num_lines <- num_lines + n
    total <- total + sum(df[[col]], na.rm = TRUE)
    message(paste(num_lines, total))
  }

  total / num_lines
}

get_mean(file_name, "h")
