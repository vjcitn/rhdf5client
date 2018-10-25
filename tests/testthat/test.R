library(rhdf5client)
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

context("HSDSSource")
test_that("Server found", {
  src.hsds <- HSDSSource('http://hsdshdflab.hdfgroup.org')
  doms <- listDomains(src.hsds, '/home/spollack')
  expect_true('/home/spollack/testzero.h5' %in% doms) 
  # catch exception: non-existent source
  src.fake <- HSDSSource('http://hsdshdflab.fdhgroup.org')
  expect_warning(listDomains(src.fake, '/home'), "bad http request")
})

context("HSDSFile")
test_that("Files can be opened for reading", {
  src.hsds <- HSDSSource('http://hsdshdflab.hdfgroup.org')
  f1 <- HSDSFile(src.hsds, '/home/spollack/testzero.h5')
  dsts <- listDatasets(f1)
  expect_true('/grpB/grpBA/dsetX' %in% dsts)
  # catch exception: non-existent or empty file
  expect_warning(HSDSFile(src.hsds, '/home/spollack/testfake.h5'), "no datasets")
})

context("HSDSDataset")
test_that("Data can be retrieved from Datasets", {
  src.hsds <- HSDSSource('http://hsdshdflab.hdfgroup.org')
  f1 <- HSDSFile(src.hsds, '/home/spollack/testone.h5')
  d1 <- HSDSDataset(f1, '/group0/dset1d')
  f2 <- HSDSFile(src.hsds, '/shared/bioconductor/tenx_full.h5')
  d2 <- HSDSDataset(f2, '/newassay001')
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

