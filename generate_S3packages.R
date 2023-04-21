# use devtools to programmatically generate R packages for testing mainly
rm(list = ls())
library(devtools)

# example: two packages with a shared S3 generic
package1_dir <- file.path(tempdir(), "package1")
if (dir.exists(package1_dir)) unlink(package1_dir, recursive = TRUE)
create(package1_dir)#, open = TRUE)

# add an R file
file1 <- "
#' @export
fish <- function(x, ...) UseMethod(\"fish\", x)

#' @exportS3Method
fish.double <- function(x, ...) x + 1

#' @exportS3Method
fish.character <- function(x, ...) paste(x, \"1\")
"
writeLines(file1, file.path(package1_dir, "R", "fish.R"))
document(package1_dir)

install(package1_dir, reload = FALSE)
try(detach(package:package1, unload = TRUE), silent = TRUE)
library(package1)
fish(1)

package2_dir <- file.path(tempdir(), "package2")
if (dir.exists(package2_dir)) unlink(package2_dir, recursive = TRUE)
create(package2_dir)#, open = TRUE)

# add an R file
file2 <- "
#' @export
fish <- function(x, ...) UseMethod(\"fish\", x)

#' @exportS3Method
fish.double <- function(x, ...) x + 2

#' @exportS3Method
fish.character <- function(x, ...) paste(x, \"2\")
"
writeLines(file2, file.path(package2_dir, "R", "fish.R"))
document(package2_dir)

install(package2_dir, reload = FALSE)
try(detach(package:package2, unload = TRUE), silent = TRUE)
library(package2) # warning we already have generic
fish(1)

# using package name calls the correct method
package1::fish(1)
package2::fish(1)

# if you add a method then it is used by both package generics in the current session
fish.logical <- function(x, ...) x + 2
methods(fish)
fish(TRUE)
package1:::fish.double(TRUE)
package1::fish(TRUE)
package2::fish(TRUE)
