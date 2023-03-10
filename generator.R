# Pattern to return a generator (iterator) function in R similar to Python
# avoids running out of memory for aggregate calculations on large data sets
# see iterators package - use generic - just add a file method for this example
rm(list = ls())
closeAllConnections()
library(readr)
library(fpeek)

# 4gb file
file_name <- 'c:/data/custom_1988_2020.csv'

peek_head(file_name)
peek_tail(file_name)

# read_csv wants binary connection using open = 'rb, but then chunks corrupted
csv_generator <- function(file_name, num_rows = 1000L, method = "read.csv", ...) {
  if (method == "read.csv") {
    con <- file(file_name, open="r")
    function() read.csv(con, nrows = num_rows, ...)
  } else if (method == "read_csv") {
    con <- file(file_name, open="rb")
    function() read_csv(con, n_max = num_rows, ...)
  } else stop('No such method')
}

close_csv_generator <- function(chunk) {
  close(environment(chunk)$con)
}

# Find mean of last column with one pass using generator
get_mean <- function(chunk, col, max_chunks = 5) {
  on.exit(close_csv_generator(chunk))

  total <- 0
  num_lines <- 0
  num_chunks <- 1

  repeat {
    df <- chunk()
    if ((n <- nrow(df)) == 0L || num_chunks > max_chunks)
      break
    num_lines <- num_lines + n
    num_chunks <- num_chunks + 1
    total <- total + sum(df[[col]], na.rm = TRUE)
    message(paste(num_lines, total))
  }

  total / num_lines
}

# chunk <- csv_generator(file_name, col.names = letters[1:8])
# get_mean(chunk, "h")

# now use iterators package ...
library(iterators)

methods(iter)

# ireadtable in iters looks more general than this simple implementation
icsv <- function(file_name, times = Inf, num_rows = 1000L, header = FALSE, ...) {
  con <- file(file_name, open="r")
  col_names <- NULL
  nextEl <- function() {
    if (times > 0) {
      tryCatch(
        df <- read.csv(con, nrows = num_rows, header = header,
                       row.names = NULL, ...),
        error = function(e) {
          close(con)
          stop('Reached EOF')
        }
      )
      times <<- times - 1
      if (header) {
        header <<- FALSE
        col_names <<- colnames(df)
      } else {
        colnames(df) <- col_names
      }
    } else {
      close(con)
      stop('Maximum chunks called')
    }
    df
  }

  obj <- list(nextElem = nextEl)
  class(obj) <- c('icsv', 'abstractiter', 'iter')
  obj
}


get_mean <- function(it, col) {
  total <- 0
  num_lines <- 0
  num_chunks <- 1

  tryCatch(
    repeat {
      df <- nextElem(it)
      num_lines <- num_lines + nrow(df)
      num_chunks <- num_chunks + 1
      total <- total + sum(df[[col]], na.rm = TRUE)
      message(paste(num_lines, total))
    },
    error = function(e) {
      message(e$message, appendLF = TRUE)
    }
  )

  total / num_lines
}

i1 <- icsv(file_name, times = 5, col.names = letters[1:8])
get_mean(i1, 'h')

# check on iris
iris_file <- 'c:/data/iris.csv'
write.csv(iris, file = iris_file, row.names = FALSE)
i2 <- icsv(iris_file, header = TRUE, num_rows = 10)
try(repeat({df <- nextElem(i2); print(df)}))

i3 <- icsv(iris_file, header = TRUE, num_rows = 10)
get_mean(i3, 1) ==  mean(iris$Sepal.Length)
