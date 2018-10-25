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

context("HSDSSources")

test_that("Both servers found", {
  src.hsds <- HSDSSource('http://hsdshdflab.hdfgroup.org')
  doms <- listDomains(src.hsds, '/home/spollack')
  expect_true('/home/spollack/testzero.h5' %in% doms) 
  src.chan <- HSDSSource('http://h5s.channingremotedata.org:5000', 'h5serv')
  doms <- listDomains(src.chan)
  expect_true('neurons100k.h5s.channingremotedata.org' %in% doms) 
  doms <- listDomains(src.chan, 'public/hdfgroup/org')
  expect_true('tall.public.h5s.channingremotedata.org' %in% doms) 
  src.fake <- HSDSSource('http://hsdshdflab.fdhgroup.org')
  # catch exception: non-existent source
  expect_warning(listDomains(src.fake, '/home'), "bad http request")
})

context("Files")
test_that("Files can be opened for reading", {
  src.hsds <- HSDSSource('http://hsdshdflab.hdfgroup.org')
  src.chan <- HSDSSource('http://h5s.channingremotedata.org:5000', 'h5serv')
  f1 <- HSDSFile(src.hsds, '/home/spollack/testzero.h5')
  dsts <- listDatasets(f1)
  expect_true('/grpB/grpBA/dsetX' %in% dsts)
  f2 <- HSDSFile(src.chan, 'tenx_100k_sorted.h5s.channingremotedata.org')
  dsts <- listDatasets(f2)
  expect_true(dsts == c('/assay001'))
  # catch exception: non-existent or empty file
  expect_warning(HSDSFile(src.hsds, '/home/spollack/testfake.h5'), "no datasets")
})

context("Datasets")
test_that("Data can be retrieved from Datasets", {
  src.hsds <- HSDSSource('http://hsdshdflab.hdfgroup.org')
  src.chan <- HSDSSource('http://h5s.channingremotedata.org:5000', 'h5serv')
  f1 <- HSDSFile(src.hsds, '/home/spollack/testone.h5')
  f2 <- HSDSFile(src.chan, 'tenx_100k_sorted.h5s.channingremotedata.org')
  d1 <- HSDSDataset(f1, '/group0/dset1d')
  d2 <- HSDSDataset(f2, '/assay001')
  R <- c(4046,2087,4654,3193)

  A <- apply(getData(d2, c('1:4', '1:27998'), transfermode='JSON'), 1, sum)
  expect_true(all(R == A))
  A <- apply(getData(d2, c('1:4', '1:27998'), transfermode='binary'), 1, sum)
  expect_true(all(R == A))
  A <- apply(d2[1:4, 1:27998], 1, sum)
  expect_true(all(R == A))
  expect_true(sum(d1[1:20]) == 937)
})

context("DelayedArray subclass HSDSArray")
test_that("DelayedArray can be instantiated and accessed",  {
  R <- c(4046,2087,4654,3193)
  da <- HSDSArray('http://h5s.channingremotedata.org:5000', 'h5serv', 
        'tenx_full.h5s.channingremotedata.org', '/newassay001')
  A <- apply(da[,1:4],2,sum)
  expect_true(all(R == A))
  da <- HSDSArray('http://hsdshdflab.hdfgroup.org', 'hsds', 
        '/shared/bioconductor/tenx_full.h5', '/newassay001')
  A <- apply(da[,1:4],2,sum)
  expect_true(all(R == A))
})

context("Four-dimensional datasets")
test_that("Higher-dimensional dataset access works correctly",  {
  src <- HSDSSource('http://hsdshdflab.hdfgroup.org')
  rd <- HSDSDataset(HSDSFile(src, '/home/spollack/testone.h5'), '/group0/group1/group2/data4d')
  A <- getData(rd, list(3:4, 8:9, 5:6, 2:3))
  expect_true(sum(A) == 697)
  dt <- HSDSDataset(HSDSFile(src, '/home/spollack/testone.h5'), '/group0/group1/dataR')
  B <- getData(dt, list(c(4), c(2, 3, 5, 6), c(5), 1:3))
  R <- array(c(3140, 3240, 3440, 3540, 3141, 3241, 3441, 3541, 3142, 
      3242, 3442, 3542), dim=c(4,3))
  expect_true(all(B == R))

})

context("Decomposition into slices")
test_that("Bad slices rejected",  {
  tf <- rhdf5client2:::checkSlices(c(10, 20, 30), c('5:', ':', ':8'))
  ok <- c('4:10:1', '0:20:1', '0:8:1')
  expect_true(all(unlist(tf) == ok))
  expect_error(rhdf5client2:::checkSlices(c(10, 20, 30), c('5:20', ':', ':8'),
    regexp='stop out of range'))
  expect_error(rhdf5client2:::checkSlices(c(10, 20, 30), c('10:5', ':', ':8'),
    regexp='slice stop less than slice start'))
  expect_error(rhdf5client2:::checkSlices(c(10, 20, 30), c('5:10,0.5', ':', ':8'),
    regexp='malformed slice'))
})

