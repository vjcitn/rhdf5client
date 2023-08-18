library(rhdf5)

h5file <- "test_string.h5"
h5createFile(h5file)
d <- as.character(1:10)
h5write(d, h5file, "d")
h5ls(h5file)

h5file <- "test_compound.h5"
h5createFile(h5file)
d <- data.frame(intCol=1:3, strCol=c("a", "b", "c"))
h5write(d, h5file, "d")
h5ls(h5file)

h5file <- "test_scalar.h5"
h5createFile(h5file)
d <- "I'm scalar"
h5write(d, h5file, "d")
h5ls(h5file)
