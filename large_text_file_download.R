# download large text file
csv.name <- "2016-04-20-processed-logs-big-file-example.csv"
db.name <- "2016-04-20-processed-logs-big-file-example.db"

library("R.utils")
# download the CSV file example
csv.url <- paste("https://s3-eu-west-1.amazonaws.com/lw-birdtracking-data/",
                 csv.name, ".gz", sep = "")
if (!file.exists(csv.name)) {
  download.file(csv.url, destfile = paste0(csv.name, ".gz"))
  gunzip(paste0(csv.name, ".gz"))
}

# download the sqlite database example
db.url <- paste("https://s3-eu-west-1.amazonaws.com/lw-birdtracking-data/",
                db.name, ".gz", sep = "")
if (!file.exists(db.name)) {
  download.file(db.url, destfile = paste0(db.name, ".gz"))
  gunzip(paste0(db.name, ".gz"))
}
