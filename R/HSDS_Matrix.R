#' simplify construction of DelayedMatrix from url and path in HSDS
#' @param url character(1) URL for HSDS object store with port
#' @param path character(1) path from root defining HDF Cloud resource
#' @return instance of DelayedArray
#' @examples
#' HSDS_Matrix
#' @export
HSDS_Matrix = function(url, path) {
  so = H5S_source(url, path)
  dss = fetchDatasets(so) 
  uu = dss$id
  if (length(uu)>1) message("multiple UUIDs found, using first")
  uu = uu[1]
  ds = H5S_dataset2(so, uu)
  DelayedArray(new("H5S_ArraySeed", filepath="", domain="",
   host="", H5S_dataset=ds))
}

