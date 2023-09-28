if (FALSE) {
context("String support")
test_that("Basic string support",  {
    src.hsds <- HSDSSource('http://hsdshdflab.hdfgroup.org')
    f <- HSDSFile(src.hsds, "/shared/NREL/sample/windspeed_z5.h5")
    d <- HSDSDataset(f, "/datetime")
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
  f <- HSDSFile(src.hsds, "/shared/ghcn/year/inventory.h5")
  d <- HSDSDataset(f, "/inventory")
  
  expect_equal(d@type$class, "H5T_COMPOUND")
  expect_equal(d@type$fields[[1]]$name, "year")
  expect_equal(d@type$fields[[1]]$type$class, "H5T_STRING")

  v1 <- d[1]  
  expect_true(is(v1, "data.frame")) # data.table inherits data.frame
  expect_equal(nrow(v1), 1)
  expect_equal(v1$year, "1763")
  expect_equal(v1$loadstart, 1636742475)
  
  v2 <- d[1:2]  
  expect_equal(nrow(v2), 2)
  expect_equal(v2$year[1], v1$year)
  expect_equal(v2$loadstart[1], v1$loadstart)
})

context("Scalar support")
test_that("Support of scalar values", {
  src.hsds <- HSDSSource('http://hsdshdflab.hdfgroup.org')
  f <- HSDSFile(src.hsds, "/shared/NASA/NCEP3/ncep3.h5")
  d <- HSDSDataset(f, "/HDFEOS INFORMATION/StructMetadata.0")
  v <- d[1] 
  expect_true(is(v, "character"))
  expect_true(startsWith(v, "GROUP"))
})
}
