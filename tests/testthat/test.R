library(rhdf5client)
context("indexing infrastructure")

test_that("sproc/isplit work", {
 if (check_hsds()) {
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
    }
  else TRUE
})

context("HSDSSource")
test_that("Server found", {
 if (check_hsds()) {
  src.hsds <- HSDSSource(URL_hsds())
  doms <- listDomains(src.hsds, '/shared/bioconductor')
  expect_true('/shared/bioconductor/patelGBMSC.h5' %in% doms) 
  # catch exception: non-existent source
  #src.fake <- HSDSSource(URL_hsds())
  #expect_warning(listDomains(src.fake, '/shared/bioconductor/'), "bad http request")
 } else TRUE
})

context("HSDSFile")
test_that("Files can be opened for reading", {
 if (check_hsds()) {
  src.hsds <- HSDSSource(URL_hsds())
  f1 <- HSDSFile(src.hsds, '/shared/bioconductor/patelGBMSC.h5')
  dsts <- listDatasets(f1)
  expect_true('/assay001' %in% dsts)
  # catch exception: non-existent or empty file
  expect_warning(HSDSFile(src.hsds, '/shared/bioconductor/tenx_nonex.h5'), "no datasets")
  } else TRUE
})

context("HSDSDataset")
test_that("Data can be retrieved from Datasets", {
 if (!check_hsds()) return(TRUE) else {
  src.hsds <- HSDSSource(URL_hsds())
  f2 <- HSDSFile(src.hsds, '/shared/bioconductor/patelGBMSC.h5')
  d2 <- HSDSDataset(f2, '/assay001')
#  R <- c(4046,2087,4654,3193)
  R <- c(1566459.51656964, 989588.912121646, 1247006.06405722, 1061847.89477033)


  A <- apply(getData(d2, c('1:4', '1:27998'), transfermode='JSON'), 1, sum)
  clRA = function(R,A) max(abs(R-A))<1e-6
  expect_true(clRA(R,A))
  A <- apply(getData(d2, c('1:4', '1:27998'), transfermode='binary'), 1, sum)
  expect_true(clRA(R,A))
  A <- apply(d2[1:4, 1:27998], 1, sum)
  expect_true(clRA(R,A))
 }
})

context("DelayedArray subclass HSDSArray")
test_that("DelayedArray can be instantiated and accessed",  {
 if (!check_hsds()) return(TRUE) else {
#  R <- c(4046,2087,4654,3193)
  R <- c(1965027.82493435, 1267166.06960898, 1627511.94926196, 1338411.18299368
)
  da <- HSDSArray(URL_hsds(), 'hsds', 
        '/shared/bioconductor/patelGBMSC.h5', '/assay001')
  A <- apply(da[,1:4],2,sum)
  clRA = function(R,A) max(abs(R-A))<1e-6
  expect_true(clRA(R,A))
 }
})

#context("Four-dimensional datasets")
#test_that("Higher-dimensional dataset access works correctly",  {
# if (!check_hsds()) return(TRUE) else {
#  src <- HSDSSource('http://hsdshdflab.hdfgroup.org')
#  rd <- HSDSDataset(HSDSFile(src, '/home/spollack/testone.h5'), '/group0/group1/group2/data4d')
#  A <- getData(rd, list(3:4, 8:9, 5:6, 2:3))
#  expect_true(sum(A) == 697)
#  dt <- HSDSDataset(HSDSFile(src, '/home/spollack/testone.h5'), '/group0/group1/dataR')
#  B <- getData(dt, list(c(4), c(2, 3, 5, 6), c(5), 1:3))
#  R <- array(c(3140, 3240, 3440, 3540, 3141, 3241, 3441, 3541, 3142, 
#      3242, 3442, 3542), dim=c(4,3))
#  expect_true(all(B == R))
# }
#})

context("Decomposition into slices")
test_that("Bad slices rejected",  {
 if (!check_hsds()) return(TRUE) else {
  tf <- rhdf5client:::checkSlices(c(10, 20, 30), c('5:', ':', ':8'))
  ok <- c('4:10:1', '0:20:1', '0:8:1')
  expect_true(all(unlist(tf) == ok))
  expect_error(rhdf5client:::checkSlices(c(10, 20, 30), c('5:20', ':', ':8'),
    regexp='stop out of range'))
  expect_error(rhdf5client:::checkSlices(c(10, 20, 30), c('10:5', ':', ':8'),
    regexp='slice stop less than slice start'))
  expect_error(rhdf5client:::checkSlices(c(10, 20, 30), c('5:10,0.5', ':', ':8'),
    regexp='malformed slice'))
  }
})

