# create and manipulate a linked list - see stack
# access using x[[c(2,3,4)]] notation
rm(list = ls())

# for last element of linked list
end_linked_list <- structure(0, class = 'EndLinkedList')

# nested list - use first element pointer then value to make
# top cheaper?
x <- list(1, list(\(x) x, end_linked_list))

# flatten linked list - see rlang too
fl <- \(z) { l <- as.list(unlist(z)); l[-length(l)] }

# push
x1 <- rapply(x, \(y) list(3, end_linked_list),
       classes = 'EndLinkedList', how = 'replace')
fl(x1)

# remove last element - pop
x2 <- rapply(x, \(y) end_linked_list,
             classes = 'EndLinkedList', how = 'replace')
fl(x2)

# top
tail(fl(x2), 1)

