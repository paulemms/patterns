# patterns to sample big data
# see https://stackoverflow.com/questions/15532810/reading-40-gb-csv-file-into-r-using-bigmemory/15798275#15798275
rm (list = ls())

library(readr)
library(fpeek)

# 4gb file
file_name <- 'c:/data/custom_1988_2020.csv'

# can hold this in memory on 16gb machine but slows ops
#full_df <- read_csv(file_name)

# find number of rows
#total_lines <- peek_count_lines(file_name)

# samples chunks of sample size using binomial dist so only one scan of file
# Repeatedly use first chunk to hold samples from subsequent chunks with smaller
# overwrites on each iteration
fsample <-
  function(fname, n, seed, header=FALSE, ..., reader = read.csv)
  {
    set.seed(seed)
    con <- file(fname, open="r")
    hdr <- if (header) {
      readLines(con, 1L)
    } else character()

    buf <- readLines(con, n)
    n_tot <- length(buf)

    repeat {
      txt <- readLines(con, n)
      if ((n_txt <- length(txt)) == 0L)
        break

      n_tot <- n_tot + n_txt
      n_keep <- rbinom(1, n_txt, n_txt / n_tot)
      if (n_keep == 0L)
        next

      keep <- sample(n_txt, n_keep)
      drop <- sample(n, n_keep)
      buf[drop] <- txt[keep]
    }

    reader(textConnection(c(hdr, buf)), header=header, ...)
  }

x <- fsample(file_name, n = 1e4, seed = 0, header = FALSE)
