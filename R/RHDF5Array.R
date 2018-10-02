#' HSDSArraySeed for HSDSArray backend to DelayedArray
#'
#' @name HSDSArraySeed
#' @slot endpoint URL of remote server
#' @slot svrtype type of server, must be either 'hsds' or 'h5serv'
#' @slot domain HDF5 domain of H5 file on server
#' @slot dsetname complete internal path to dataset in H5 file
#' @slot dataset object of type HSDSDataset for access to the H5 dataset
#' @aliases HSDSArraySeed-class
#' @import DelayedArray
#' @exportClass HSDSArraySeed
setClass("HSDSArraySeed", contains=c("Array"), 
  slots = c(endpoint="character",    # URL
            svrtype="character",     # 'h5serv' or 'hsds'
            domain="character",      # extra-file (file system) path
            dsetname="character",    # complete intra-file path
            dataset="HSDSDataset")       # HSDSDataset object 
)

#' Construct an object of type HSDSArraySeed 
#' 
#' @name HSDSArraySeed
#' 
#' @param endpoint URL of remote server
#' @param svrtype type of server, must be either 'hsds' or 'h5serv'
#' @param domain HDF5 domain of H5 file on server
#' @param dsetname complete internal path to dataset in H5 file
#' @return An initialized object of type HSDSArraySeed
#' @export 
HSDSArraySeed <- function(endpoint, svrtype, domain, dsetname)  {
  src <- rhdf5client2::HSDSSource(endpoint, svrtype)
  fle <- rhdf5client2::HSDSFile(src, domain)
  dset <- rhdf5client2::HSDSDataset(fle, dsetname)
  new("HSDSArraySeed", endpoint=endpoint, svrtype=svrtype, domain=domain, 
      dsetname=dsetname, dataset=dset)
}

#' Obtain names of dimensions for an object of type HSDSArraySeed
#' 
#' (required by DelayedArray seed contract, returns NULL list)
#' 
#' @name dimnames
#' @param x An object of type HSDSArraySeed
#' @return A NULL list of length equal to the array dimensionality
#' @aliases dimnames,HSDSArraySeed-method
#' @export
setMethod("dimnames", "HSDSArraySeed", function(x)  {
  n <- length(x@dataset@shape)
  rt <- vector(mode="list", length=n)   # null list
})

#' Obtain dimensions of an object of type HSDSArraySeed
#' 
#' (required by DelayedArray seed contract)
#' HDF server content is assumed transposed relative to R matrix layout.
#' This anticipates H5 datasets on the server with rows for 
#' experimental samples and columns for *-omic features. The 
#' Bioconductor SummarizedExperiment requires *-omic features in 
#' rows and samples in columns.
#' 
#' @name dim
#' @param x An object of type HSDSArraySeed
#' @return A numeric vector of the dimensions
#' @aliases dim,HSDSArraySeed-method
#' @export
setMethod("dim", "HSDSArraySeed", function(x)  {
   as.integer(rev(x@dataset@shape))   # Could be this??? Don't assign?
})

#' Access dataset backed by an HSDSArraySeed
#'
#' @name extract_array
#' @param x An object of type HSDSArraySeed
#' @param index A list of numeric vectors to be accessed, one vector 
#' for each dimension of the array object. A NULL vector indicates
#' the entire range of indices in that dimension. A zero-length
#' vector indicates no indices in the relevant dimension. (Accordingly,
#' any zero-length vector of indices will result in an empty array
#' being returned.)
#' 
#' @return An array containing the data elements corresponding to the 
#' indices requested
#' @aliases extract_array,HSDSArraySeed-method
#' 
#' @export
#'
# TODO: seed contract requires repeated and descending indices 
# (e.g., c(1,2,3,3,2,1)) be returned correctly. 
setMethod("extract_array", "HSDSArraySeed", function(x, index)  {
  index <- rev(index)

  # two special cases
  # (i) NULL index - signifies all elements in this dimension
  # (ii) zero-length index - signifies zero elements in this dimension
  # which means a null fetch 

  idxlist <- lapply(seq_along(index), 
    function(i)  {
      if (is.null(index[[i]]))  {
        n <- x@dataset@shape[i]
        if (n == 0)  {
          v <- numeric(0)
        } else  {
          v <- seq(1,n)
        }
      } else if (length(index[[i]]) == 0)  {
        v <- numeric(0)
      } else  {
        v <- unlist(slicelst(index[[i]]))
      }
      v
    }) 

  rdims <- lapply(idxlist, function(v) length(v))
  nullfetch <- any(rdims == 0)
  if (nullfetch) { 
    A <- array(numeric(0), dim=rdims)
  } else  {
    A <- rhdf5client2:::getDataList(x@dataset, idxlist)
    # unflatten vector if necessary: see note at 
    # end of Dataset::getDataVec on flattened result
    # for fetch of single-width dimensioned arrays.
    A <- array(A, dim=rdims)
  }
  R <- t(A)   # untranspose the transpose
  R
})

#' A DelayedArray backend for accessing a remote HDF5 server.
#' 
#' @name HSDSArray 
#' @family  HSDSArray
#' @aliases HSDSArray-class
#' @exportClass HSDSArray
setClass("HSDSArray", contains="DelayedArray")

#' DelayedMatrix subclass for a two-dimensional HSDSArray
#'
#' @name HSDSMatrix
#' @family  HSDSArray
#' @aliases HSDSMatrix-class
#' @exportClass HSDSMatrix
setClass("HSDSMatrix", contains=c("DelayedMatrix", "HSDSArray"))

setMethod("matrixClass", "HSDSArray", function(x) "HSDSMatrix")

setAs("HSDSArray", "HSDSMatrix", function(from)  { 
    new("HSDSMatrix", from)
  }
)

#' Coercion method from HSDSMatrix to its superclass HSDSArray
#'
#' @name as
#' @family  HSDSArray

setAs("HSDSMatrix", "HSDSArray", function(from) from)   # no-op

#' @importFrom DelayedArray new_DelayedArray
setMethod("DelayedArray", "HSDSArraySeed", 
  function(seed)   
    new_DelayedArray(seed, Class="HSDSArray") 
)

#' Construct an object of type HSDSArray directly from the data
#' members of its seed
#'
#' @param endpoint URL of remote server
#' @param svrtype type of server, must be either 'hsds' or 'h5serv'
#' @param domain HDF5 domain of H5 file on server
#' @param dsetname complete internal path to dataset in H5 file
#' @return An initialized object of type HSDSArray
#' @export
HSDSArray <- function(endpoint, svrtype, domain, dsetname)  {
  DelayedArray(HSDSArraySeed(endpoint, svrtype, domain, dsetname))
}
