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
  src.hsds <- HSDSSource('http://hsdshdflab.hdfgroup.org')
  doms <- listDomains(src.hsds, '/shared/bioconductor')
  expect_true('/shared/bioconductor/tenx_full.h5' %in% doms) 
  # catch exception: non-existent source
  src.fake <- HSDSSource('http://hsdshdflab.fdhgroup.org')
  expect_warning(listDomains(src.fake, '/shared/bioconductor/'), "bad http request")
 } else TRUE
})

context("HSDSFile")
test_that("Files can be opened for reading", {
 if (check_hsds()) {
  src.hsds <- HSDSSource('http://hsdshdflab.hdfgroup.org')
  f1 <- HSDSFile(src.hsds, '/shared/bioconductor/tenx_full.h5')
  dsts <- listDatasets(f1)
  expect_true('/newassay001' %in% dsts)
  # catch exception: non-existent or empty file
  expect_error(HSDSFile(src.hsds, '/shared/bioconductor/tenx_nonex.h5'), "Not Found")
  } else TRUE
})

context("HSDSDataset")
test_that("Data can be retrieved from Datasets", {
 if (!check_hsds()) return(TRUE) else {
  src.hsds <- HSDSSource('http://hsdshdflab.hdfgroup.org')
  f2 <- HSDSFile(src.hsds, '/shared/bioconductor/tenx_full.h5')
  d2 <- HSDSDataset(f2, '/newassay001')
  R <- c(4046,2087,4654,3193)

  A <- apply(getData(d2, c('1:4', '1:27998'), transfermode='JSON'), 1, sum)
  expect_true(all(R == A))
  A <- apply(getData(d2, c('1:4', '1:27998'), transfermode='binary'), 1, sum)
  expect_true(all(R == A))
  A <- apply(d2[1:4, 1:27998], 1, sum)
  expect_true(all(R == A))
 }
})

context("DelayedArray subclass HSDSArray")
test_that("DelayedArray can be instantiated and accessed",  {
 if (!check_hsds()) return(TRUE) else {
  R <- c(4046,2087,4654,3193)
  da <- HSDSArray('http://hsdshdflab.hdfgroup.org', 'hsds', 
        '/shared/bioconductor/tenx_full.h5', '/newassay001')
  A <- apply(da[,1:4],2,sum)
  expect_true(all(R == A))
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


context("String support")
test_that("Basic string support",  {
    src.hsds <- HSDSSource('http://hsdshdflab.hdfgroup.org')
    f <- HSDSFile(src.hsds, "/shared/test_string.h5")
    d <- HSDSDataset(f, "/d")
    expect_true(d@type$class == "H5T_STRING")
    
    v1 <- d[1:10]
    expect_equal(class(v1), "character")
    
    v2 <- d[1]
    expect_equal(class(d[1]), "character")
    expect_equal(v1[1], v2)
})

context("Compound support")
test_that("Basic compound support", {
  src.hsds <- HSDSSource('http://hsdshdflab.hdfgroup.org')
  f <- HSDSFile(src.hsds, "/shared/test_compound.h5")
  d <- HSDSDataset(f, "/d")
  
  expect_equal(d@type$class, "H5T_COMPOUND")
  expect_equal(d@type$fields[[1]]$name, "intCol")
  expect_equal(d@type$fields[[1]]$type$class, "H5T_INTEGER")
  expect_equal(d@type$fields[[2]]$name, "strCol")
  expect_equal(d@type$fields[[2]]$type$class, "H5T_STRING")

  v1 <- d[1]  
  expect_true(is(v1, "data.frame")) # data.table inherits data.frame
  expect_equal(nrow(v1), 1)
  expect_equal(v1$intCol, 1)
  expect_equal(v1$strCol, "a")
  
  v2 <- d[1:2]  
  expect_equal(nrow(v2), 2)
  expect_equal(v2$intCol[1], v1$intCol)
  expect_equal(v2$strCol[1], v1$strCol)
  
  typ <- list(class="H5T_COMPOUND", 
              fields=list(
                list(name="f1", type=list(class="H5T_STRING")),
                list(name="f2", type=list(class="H5T_STRING")),
                list(name="f3", type=list(class="H5T_STRING"))
              ))
  # JSON arrays are used when all columns are strings
  str <- c('[["asd", "qwe", "zxc"], ["a", "b", "c"]]')
  dt <- extractCompoundJSON(type = typ, rjson::fromJSON(str))
  expect_true(is(dt, "data.frame"))
})

context("Scalar support")
test_that("Support of scalar values", {
  src.hsds <- HSDSSource('http://hsdshdflab.hdfgroup.org')
  f <- HSDSFile(src.hsds, "/shared/test_scalar.h5")
  d <- HSDSDataset(f, "/d")
  v <- d[1] 
  expect_true(is(v, "character"))
  expect_identical(v, "I'm scalar")
})

test_that("Request errors are reported", {
  if (!check_hsds()) return(TRUE) else {
    src.hsds <- HSDSSource("https://developer.nrel.gov/api/hsds")
    expect_error(HSDSFile(src.hsds, "/shared/NASA/NCEP3/ncep3.h5"), "api_key")
  }
})

