library(rhdf5client)
context("connection")

test_that("H5S_source completes", {
 bigec2 = H5S_source(URL_h5serv())
 expect_true(is(bigec2, "H5S_source"))
})

context("content wrapper structure") 

test_that("H5S_source processes", {
 bigec2 = H5S_source(URL_h5serv())
 expect_true(all(dim(groups(bigec2))==c(2,2))) 
 expect_true(is(links(bigec2,1), "H5S_linkset"))
 expect_true(is(dataset(bigec2, "tenx_100k"), "H5S_dataset"))
 expect_true(is(bigec2[["tenx_100k"]], "H5S_dataset"))
 expect_true(is(dsmeta(bigec2), "DataFrame"))
 expect_true(all(dim(dsmeta(bigec2))==c(2,3))) 
})

context("indexing infrastructure")

test_that("sproc/isplit work", {
 expect_true(length(isplit(c(1,2,3,4,5,10,15,20,30:40)))==3)
 ii = isplit(c(1,2,3,4,5,10,15,20,30:40))
 ss = structure(c("0:5:1", "9:20:5", "29:40:1"), .Names = c("1", 
"2", "3"))
 expect_true(identical(ss, unlist(sproc(ii))))
 ii = isplit(c(1:10, seq(50,25,-5), seq(80,100,2)))
 ss = structure(c("0:10:1", "50:24:-5", "79:100:2"), 
   .Names = c("1", "2", "3"))
 expect_true(identical(ss, unlist(sproc(ii))))
 ii = isplit(c(1, 3, 5, 200000, 300000))
 ss = structure(c("0:5:2", "199999:300000:100000"), 
   .Names = c("1", "2"))
 expect_true(identical(ss, unlist(sproc(ii))))

})

context("targets generation")

test_that("targets method works", {
 bigec2 = H5S_source(URL_h5serv())
 tt = targets(links(bigec2, 1))
 expect_true(length(tt)>=10)     # increased from 7 because tenx_400k added
 expect_true(length(grep("host", tt))>=8) 
})

context("retrieving data with binary transfer")

test_that("binary transfer works", {
 bigec2 <- H5S_source(URL_h5serv())
 txdat <- bigec2[["tenx_100k_sorted"]]

 transfermode(txdat) <- "JSON"
 J <- txdat[15:20, 1905:1906]
 transfermode(txdat) <- "binary"
 B <- txdat[15:20, 1905:1906]
 expect_true(all(J == B))

 N <- matrix(c(40, 35, 13, 118, 25, 26, 1, 0, 1, 2, 1, 1), nrow=6, ncol=2, byrow=FALSE)
 expect_true(all(J == N))

 M <- txdat[ c(-(1:79999),-(80500:100000)), 4000:4500 ]
 im <- which(M > 200)
 N <- c(378, 271, 255, 204, 1188, 280, 1458, 884, 947)
 expect_true(all(M[im] == N))
})





