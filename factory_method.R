# Implementation of the factory method pattern
rm(list = ls())

# With a functional approach we write a generic function that dispatches
# on the type of key

create_encryption <- function(key, ...) {
  UseMethod("encryption", key, ...)
}

create_encryption.DES <- function(key, ...) {
  list(
    decrypt = function(in_stream) {},
    encrpyt = function(out_stream) {}
  )
}

create_encryption.RSA <- function(key, ...) {
  list(
    decrypt = function(in_stream) {},
    encrpyt = function(out_stream) {}
  )
}


encrypted_socket <- function(key, socket) {
  crypt <- create_encryption(key)
  list(
    get_input_stream = function() crypt$encrypt(socket$get_input_stream()),
    get_output_stream = function() crypt$decrypt(socket$get_output_stream())
  )
}
