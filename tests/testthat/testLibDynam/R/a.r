a <- 1

nulltest3 <- function() {
  .Call("null_test", PACKAGE = "testDllLoad")
}

nulltest4 <- function() {
  .Call(null_test2)
}
