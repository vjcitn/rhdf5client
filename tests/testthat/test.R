library(rhdf5client)

context("connection")

test_that("H5S_source completes", {
 bigec2 = H5S_source("http://54.174.163.77:5000")
 expect_true(is(bigec2, "H5S_source"))
})

context("content wrapper structure") 

test_that("H5S_source processes", {
 bigec2 = H5S_source("http://54.174.163.77:5000")
 expect_true(all(dim(groups(bigec2))==c(10,2))) 
 expect_true(is(links(bigec2,1), "H5S_linkset"))
 expect_true(is(dataset(bigec2, "tenx_100k"), "H5S_dataset"))
 expect_true(is(bigec2[["tenx_100k"]], "H5S_dataset"))
 expect_true(is(dsmeta(bigec2), "DataFrame"))
 expect_true(all(dim(dsmeta(bigec2))==c(10,3))) 
})

context("indexing infrastructure")

test_that("sproc/isplit work", {
 expect_true(length(isplit(c(1,2,3,4,5,10,15,20,30:40)))==4)
 ii = isplit(c(1,2,3,4,5,10,15,20,30:40))
 ss = structure(c("0:5:1", "9:20:5", "29:30:1", "30:40:1"), .Names = c("1", 
"2", "3", "4"))
 expect_true(identical(ss, unlist(sproc(ii))))
})

context("targets generation")

test_that("targets method works", {
 bigec2 = H5S_source("http://54.174.163.77:5000")
 tt = targets(links(bigec2, 1))
 expect_true(length(tt)==9)     # increased from 7 because tenx_400k added
 expect_true(length(grep("host", tt))==7) 
})

context("retrieving data with binary transfer")

test_that("binary transfer works", {
 bigec2 <- H5S_source("http://54.174.163.77:5000")
 txdat <- bigec2[["tenx_100k_sorted"]]
 M <- txdat[ 15:20, 1905:1906 ]
 N <- matrix(c(40, 35, 13, 118, 25, 26, 1, 0, 1, 2, 1, 1), nrow=6, ncol=2, byrow=FALSE)
 expect_true(all(M == N))
})
