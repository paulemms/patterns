# use a buffered expanding ist to get better performing appends

# from https://inbo.github.io/tutorials/tutorials/r_large_data_files_handling/
# https://stackoverflow.com/questions/2436688/append-an-object-to-a-list-in-r-in-amortized-constant-time-o1

rm(list = ls())
csv.name <- "c:/data/custom_1988_2020.csv"

library(ggplot2)
library(data.table)
library(readr)

# expand a list in O(1) time by using a buffer
expanding_list <- function(capacity = 10) {
  buffer <- vector('list', capacity)
  length <- 0

  methods <- list()

  methods$double.size <- function() {
    buffer <<- c(buffer, vector('list', capacity))
    capacity <<- capacity * 2
  }

  methods$add <- function(val) {
    if(length == capacity) {
      methods$double.size()
    }

    length <<- length + 1
    buffer[[length]] <<- val
  }

  methods$as.list <- function() {
    b <- buffer[0:length]
    return(b)
  }

  methods
}


streamFile1 <- function(limit) {
  con <- file(csv.name, open = "r")
  selectedRecords <- list()
  i <- 0
  file.streaming.timing <- system.time(
    while (i < limit) {
      oneLine <- readLines(con, n = 1, warn = FALSE)
      vec = strsplit(oneLine, ",")
      selectedRecords <- c(selectedRecords, vec) # slow
      i <- i + 1
    }
  )
  close(con)
  return(file.streaming.timing[[3]])
}

# expanding list
streamFile2 <- function(limit) {
  con <- file(csv.name, open = "r")
  selectedRecords <- expanding_list(100)
  i <- 0
  file.streaming.timing <- system.time(
    while (i < limit) {
      oneLine <- readLines(con, n = 1, warn = FALSE)
      vec = strsplit(oneLine, ",")[[1]]
      selectedRecords$add(vec) # O(1)
      i <- i + 1
    }
  )
  selected_records <- selectedRecords$as.list()
  close(con)
  return(file.streaming.timing[[3]])
}

# preallocate and assign using index
streamFile3 <- function(limit) {
  con <- file(csv.name, open = "r")
  selectedRecords <- vector("list", limit)
  i <- 0
  file.streaming.timing <- system.time(
    while (i < limit) {
      oneLine <- readLines(con, n = 1, warn = FALSE)
      vec = strsplit(oneLine, ",")[[1]]
      selectedRecords[[i + 1]] <- vec # seems fastest way atm
      i <- i + 1
    }
  )
  close(con)
  return(file.streaming.timing[[3]])
}

freadFile <- function(limit) {
  file.fread.timing = system.time(
    d <- fread(csv.name, showProgress = FALSE, nThread = 2, nrows = limit)
  )
  return(file.fread.timing[[3]])
}

readrFile <- function(limit) {
  file.readr.timing = system.time(
    d <- read_csv(csv.name, progress = FALSE, num_threads = 2, col_types = cols(), n_max = limit)
  )
  return(file.readr.timing[[3]])
}

maxLines <- seq(5000, 100000, by = 5000)
streamingTimes1 <- sapply(maxLines, streamFile1)
streamingTimes3 <- sapply(maxLines, streamFile3)
streamingTimes2 <- sapply(maxLines, streamFile2)
freadTimes <- sapply(maxLines, freadFile)
readrTimes <- sapply(maxLines, readrFile)
data <- data.frame(n = maxLines, streaming1 = streamingTimes1,
                   streaming2 = streamingTimes2, streaming3 = streamingTimes3,
                   fread = freadTimes, readr = readrTimes)
pdata <- melt(as.data.table(data), id = c("n"))
colnames(pdata) <- c("n", "algorithm", "execTime")
qplot(n, execTime, data = pdata, color = algorithm, log = "y",
      xlab = "number of lines read", ylab = "log execution time (s)")
