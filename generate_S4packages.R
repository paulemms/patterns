# use devtools to programmatically generate R packages for testing mainly
rm(list = ls())
library(devtools)

# example: two packages with a S4 generic of same name
package1_dir <- file.path(tempdir(), "package1")
if (dir.exists(package1_dir)) unlink(package1_dir, recursive = TRUE)
create(package1_dir)#, open = TRUE)

# add an R file
file1 <- r"(
#' @export
setGeneric("fish", function(x) NULL) # default method as second arg

#' @export
setMethod("fish", signature(x = "numeric"), function(x) x + 1) # use numeric rather than double in S4 classes
)"
writeLines(file1, file.path(package1_dir, "R", "fish.R"))
document(package1_dir)

install(package1_dir, reload = FALSE)
try(detach(package:package1, unload = TRUE), silent = TRUE)
library(package1)
fish # has a reference to the package where it is defined

package2_dir <- file.path(tempdir(), "package2")
if (dir.exists(package2_dir)) unlink(package2_dir, recursive = TRUE)
create(package2_dir)#, open = TRUE)

# add an R file
file2 <- r"(
#' @export
setGeneric("fish", function(x) NULL) # default method as second arg

#' @export
setMethod("fish", signature(x = "numeric"), function(x) x + 2) # use numeric rather than double in S4 classes
)"
writeLines(file2, file.path(package2_dir, "R", "fish.R"))
document(package2_dir)

install(package2_dir, reload = FALSE)
try(detach(package:package2, unload = TRUE), silent = TRUE)
library(package2) # masks fish from package1
fish # has a reference to the package where it is defined
fish(1)

# using package name calls the correct method
package1::fish(1)
package2::fish(1)

# if you add a method then it is added to package currently loaded
setMethod("fish", signature(x = "logical"), function(x) x + 2)
methods(fish) # methods are found in all packages on the search path
package1::fish(TRUE) # not defined in package1
package2::fish(TRUE)

try(detach(package:package1, unload = TRUE), silent = TRUE)
try(detach(package:package2, unload = TRUE), silent = TRUE)
library(package2)
library(package1)
setMethod("fish", signature(x = "logical"), function(x) x + 2)
package1::fish(TRUE)
package2::fish(TRUE) # not defined in package2

# add methods directly to package generics
setMethod(package1::fish, signature(x = "logical"), function(x) x + 4)
setMethod(package2::fish, signature(x = "logical"), function(x) x + 5)
fish(TRUE)
package1::fish(TRUE)
package2::fish(TRUE)
