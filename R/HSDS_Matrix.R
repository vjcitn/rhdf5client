deprecate_msg = paste0("This function is deprecated. The new interface to rhdf5client",
 " is exclusively through its DelayedArray backend HSDSArray")

#' simplify construction of DelayedMatrix from url and path in HSDS
#'
#' This class is deprecated and will be defunct in the next release.
#'
#' @param url character(1) URL for HSDS object store with port
#' @param path character(1) path from root defining HDF Cloud resource
#' @return instance of DelayedArray
#' @examples
#' HSDS_Matrix
#' @export
HSDS_Matrix_OLD = function(url, path) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  so = H5S_source(url, path)
  dss = fetchDatasets(so) 
  uu = dss$id
  if (length(uu)>1) message("multiple UUIDs found, using first")
  uu = uu[1]
  ds = H5S_dataset2(so, uu)
  DelayedArray(new("H5S_ArraySeed", filepath="", domain="",
   host="", H5S_dataset=ds))
}

#' simplify construction of DelayedMatrix from url and path in HSDS
#'
#' This class is deprecated and will be defunct in the next release.
#'
#' @param url character(1) URL for HSDS object store with port
#' @param path character(1) path from root defining HDF Cloud resource
#' @param title character(1) name of dataset to use
#' @return instance of DelayedArray
#' @examples
#' HSDS_Matrix(URL_hsds(), "/shared/bioconductor/darmgcls.h5")
#' @export
HSDS_Matrix = function(url, path, title) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  so = H5S_source(url, path)
  dss = fetchDatasets(so)
  uu = dss$id
  if (length(uu)>1) {
    message("multiple UUIDs found")
    if (missing(title)) {
      message("no title provided, using first UUID")
      uu = uu[1]
      }
    else {
      ind = which(dss$title == title)
      if (length(ind)==1) uu = uu[ind]
        else stop("title does not pick out a single dataset to use")
      }
    }
  ds = H5S_dataset2(so, uu)
  if (length(ds@shapes$dims)==1) {
       targ = gsub("&.*", "", ds@presel)
       gg = fromJSON(readBin(GET(targ)$content,what="character"))$value
       message("one dimensional response, returning text")
       return(gg)
       }
  DelayedArray(new("H5S_ArraySeed", filepath="", domain="",
   host="", H5S_dataset=ds))
}

