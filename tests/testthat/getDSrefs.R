# 30 August 2018 -- purpose of this code is
# to verify that key subsetting code related to
# H5S_dataset2 does not regress
#
getDSrefs = function(url = rhdf5client::URL_hsds(), 
   dompath = "/shared/bioconductor/htxcomp_genes.h5") {
 ini = httr::GET(paste0(url, "/datasets?domain=", dompath))
 val = try(rjson::fromJSON(readBin(ini$content, "text")))
 if (inherits(val, "try-error")) {
   print(val)
   stop("GET /datasets failed to produce useful content.")
   }
 groups = httr::GET(paste0(url, "/groups?domain=", dompath))
 groups.val = try(rjson::fromJSON(readBin(groups$content, "text")))
 if (inherits(groups.val, "try-error")) {
   print(val)
   stop("GET /groups failed to produce useful content.")
   }
 gtab = do.call(rbind.data.frame, c(groups.val$hrefs, stringsAsFactors=FALSE))
 rownames(gtab) = gtab$rel
 prelink = gtab["root","href"]
 lurl = sub("\\?", "/links?", prelink)
 links = httr::GET(lurl)
 links.val = try(rjson::fromJSON(readBin(links$content, "text")))
 if (inherits(links.val, "try-error")) {
   print(val)
   stop("GET /groups/.../links failed to produce useful content.")
   }
 ltab = do.call(rbind.data.frame, c(links.val$links, stringsAsFactors=FALSE))
 ltab
}

retrieveDataset = function(url=rhdf5client::URL_hsds(), dompath =
  "/shared/bioconductor/darmgcls.h5", title="assay001") {
#
# function will attempt to return a DelayedMatrix with
# contents given by the [title] element of the [dompath]
# resource, unless the [title] element is one-dimensional,
# in which case a character vector is returned
#
 s1 = getDSrefs(url, dompath=dompath)
 uu = s1[which(s1$title == title), "id"]
 so = rhdf5client::H5S_source(url, dompath)
 suppressMessages({  # encoding message
 ds = rhdf5client::H5S_dataset2(so, uu)
 })
 if (length(ds@shapes$dims) == 1) {
        targ = gsub("&.*", "", ds@presel)
        gg = rjson::fromJSON(readBin(GET(targ)$content, w = "character"))$value
        message("one dimensional response, returning text")
        return(gg)
    }
 DelayedArray::DelayedArray(new("H5S_ArraySeed", filepath = "", domain = "", 
         host = "", H5S_dataset = ds))
}
 
context("H5S_dataset2 exercises")

test_that("darmgcls.h5 can be retrieved from kitalab", {
 url = URL_hsds()
 dompath = "/shared/bioconductor/darmgcls.h5"
 title = "assay001"
 s1 = try(getDSrefs(url = url, dompath = dompath))
 if (inherits(s1, "try-error")) {
   print(s1)
   fail(paste("getDSrefs fails on", dompath))
 }
 uu = s1[which(s1$title == title), "id"]
 expect_true(length(uu)==1)
 so = rhdf5client::H5S_source(url, dompath)
 suppressMessages({  # encoding message
 ds = rhdf5client::H5S_dataset2(so, uu)
 })
 expect_true(is(ds, "H5S_dataset"))
 ans = DelayedArray::DelayedArray(new("H5S_ArraySeed", 
         filepath = "", domain = "", 
         host = "", H5S_dataset = ds))
 expect_true(all(dim(ans) == c(65218, 3584)))

# compute indices of corners of matrix

corn = function(m, siz=3) {
 r = nrow(m)
 c = ncol(m)
 rr = unlist(lapply(list(head,tail), function(f) f(1:r, siz)))
 cc = unlist(lapply(list(head,tail), function(f) f(1:c, siz)))
 list(rr, cc)
}

mcorn = function(m, siz=4) { 
  cc = corn(m, siz=siz)
  m[cc[[1]], cc[[2]]]
}

expect_equal(sum(mcorn(ans)), 55928.6253033)
})

context("use retrieveDataset to get rownames")

test_that("retrieveDataset gets rownames", {
   rn = retrieveDataset(url=rhdf5client::URL_hsds(), dompath =
       "/shared/bioconductor/htxcomp_genes.h5", title="rownames") 
   expect_equal(length(rn), 58288)
   expect_true(all.equal(head(rn), c("ENSG00000000003.14", "ENSG00000000005.5", 	"ENSG00000000419.12", "ENSG00000000457.13", 
        "ENSG00000000460.16", "ENSG00000000938.12")))
})
