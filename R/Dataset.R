deprecate_msg = paste0("This function is deprecated. The new interface to rhdf5client",
 " is exclusively through its DelayedArray backend HSDSArray")

#' An S4 class to represent a dataset in a HDF5 file.
#' @import httr methods rjson
#' @slot file An object of type HSDSFile; the file in which the dataset is resident.
#' @slot path The dataset's path in the internal HDF5 hiearchy.
#' @slot uuid The unique unit ID by which the dataset is accessed in the server 
#' database system.
#' @slot shape The dimensions of the dataset
#' @slot type The dataset's HDF5 datatype
setClass("HSDSDataset", representation(file="HSDSFile", path="character", uuid="character",
  shape="numeric", type="list"))

#' Construct an object of type HSDSDataset 
#' A HSDSDataset is a representation of a dataset in a HDF5 file.
#' @name HSDSDataset
#' @param file An object of type HSDSFile which hosts the dataset 
#' @param path The complete intrafile path to the dataset
#' @return An initialized object of type HSDSDataset
#' @examples
#' src <- HSDSSource('http://hsdshdflab.hdfgroup.org')
#' f <- HSDSFile(src, '/home/spollack/testzero.h5')
#' d <- HSDSDataset(f, '/grpA/grpAB/dsetX')
#' @export
HSDSDataset <- function(file, path)  {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  idx <- which(file@dsetdf[,1] == path)
  if (length(idx) == 0)  
    stop("no such dataset")
  uuid <- file@dsetdf[idx,2]
  request <- paste0(file@src@endpoint, '/datasets/', uuid, '?domain=', file@domain)
  response <- tryCatch(submitRequest(request),
    error=function(e) { NULL })
  if (is.null(response))  { # this should be almost impossible
    warning("bad http request", call. = FALSE)
    return(NULL)
  }
  shape <- response$shape$dims
  type <- list(class=response$type$class, base=response$type$base)

  new("HSDSDataset", file=file, path=path, uuid=uuid,
    shape=shape, type=type)
}

.HSDSDataset <- function(file, path)  { # after deprecation cycle this is all that will remain
  idx <- which(file@dsetdf[,1] == path)
  if (length(idx) == 0)  
    stop("no such dataset")
  uuid <- file@dsetdf[idx,2]
  request <- paste0(file@src@endpoint, '/datasets/', uuid, '?domain=', file@domain)
  response <- tryCatch(submitRequest(request),
    error=function(e) { NULL })
  if (is.null(response))  { # this should be almost impossible
    warning("bad http request", call. = FALSE)
    return(NULL)
  }
  shape <- response$shape$dims
  type <- list(class=response$type$class, base=response$type$base)

  new("HSDSDataset", file=file, path=path, uuid=uuid,
    shape=shape, type=type)
}

#' extract elements of a one or two-dimensional HSDSDataset
#' 
#' Fetch data from a remote dataset
#'
#'
#' The servers require data to be fetched in slices, i.e., in sets of 
#' for which the indices of each dimension are of the form start:stop:step.
#' More complex sets of indices will be split into slices and fetched in
#' multiple requests. This is opaque to the user, but may enter into 
#' considerations of data access patterns, e.g., for performance-tuning.
#'
#' @param dataset An object of type HSDSDataset, the dataset to access.
#'
#' @param indices The indices of the data to fetch
#'
#' @param transfermode Either (default) 'JSON' or 'binary'
#'
#' @return an Array containing the data fetched from the server
#'
#' @docType methods
#' @rdname getData-methods
#'
#' @examples
#' s <- HSDSSource('http://hsdshdflab.hdfgroup.org')
#' f <- HSDSFile(s, '/shared/bioconductor/tenx_full.h5')
#' d <- HSDSDataset(f, '/newassay001')
#' x <- getData(d, c('1:4', '1:27998'), transfermode='JSON')
#' # x <- getData(d, c(1:4, 1:27998), transfermode='JSON') # method missing?
#' x <- d[1:4,1:27998]
#' @export 
setGeneric("getData", function(dataset, indices, transfermode) standardGeneric("getData"))
#
# need private version after deprecation cycle
#
setGeneric(".getData", function(dataset, indices, transfermode) standardGeneric(".getData"))

#' @rdname getData-methods
#' @aliases getData,HSDSDataset,character,character-method
setMethod("getData", c("HSDSDataset", "character", "character"),  
  function(dataset, indices, transfermode)  {
    #.Deprecated("HSDSArray", NULL, deprecate_msg)
    getDataVec(dataset, indices, transfermode)
  })

#private for after cycle
setMethod(".getData", c("HSDSDataset", "character", "character"),  
  function(dataset, indices, transfermode)  {
    getDataVec(dataset, indices, transfermode)
  })

#' @rdname getData-methods
#' @aliases getData,HSDSDataset,character,missing-method
setMethod("getData", c("HSDSDataset", "character", "missing"),  
  function(dataset, indices)  {
    #.Deprecated("HSDSArray", NULL, deprecate_msg)
    getDataVec(dataset, indices, 'JSON')
  })
#private for after cycle
setMethod(".getData", c("HSDSDataset", "character", "missing"),  
  function(dataset, indices)  {
    getDataVec(dataset, indices, 'JSON')
  })

#' @rdname getData-methods
#' @aliases getData,HSDSDataset,list,character-method
setMethod("getData", c("HSDSDataset", "list", "character"),  
function(dataset, indices, transfermode)  {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  getDataList(dataset, indices, transfermode)  
  })

#private for after cycle
setMethod(".getData", c("HSDSDataset", "list", "character"),  
function(dataset, indices, transfermode)  {
  getDataList(dataset, indices, transfermode)  
  })

#' @rdname getData-methods
#' @aliases getData,HSDSDataset,list,,missing-method
setMethod("getData", c("HSDSDataset", "list", "missing"),  
function(dataset, indices)  {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  getDataList(dataset, indices, 'JSON')  
  })


# private - perform a single fetch; indices is a vector of
# type character with one slice per dimension.
#' @useDynLib rhdf5client, .registration = TRUE
getDataVec <- function(dataset, indices, transfermode = 'JSON')  {

    indices <- checkSlices(dataset@shape, indices)
    if (length(indices) == 0)
      stop("bad slices")
    if (!(transfermode %in% c('JSON', 'binary')))  {
      warning('unrecognized transfermode, using JSON')
      transfermode <- 'JSON'
    }

    sdims <- vapply(indices, slicelen, numeric(1))

    # rdims is dimensions of response$value
    singlefetch <- FALSE
    if (all(sdims == 1))  {
      # this is a single-value fetch
      rdims <- c(1)
      singlefetch <- TRUE
    } else  {
      # h5pyd drops single-width dimensions at the end. 
      # dimensions of the result
      rdims <- sdims[which(sdims != 1)]
      while(sdims[length(sdims)] == 1)
        sdims <- sdims[1:(length(sdims)-1)]
    }

    indices <- vapply(indices, function(s) { s }, character(1))
    sel <- paste0('[', paste(indices, collapse=','), ']')
    endpoint <- dataset@file@src@endpoint
    domain <- dataset@file@domain

    request <- paste0(endpoint, '/datasets/', dataset@uuid, 
      '/value?domain=', domain, '&select=', sel)
    response <- submitRequest(request, transfermode=transfermode)

    if (singlefetch)  {  
      return(as.numeric(response$value))
    }

    if (length(rdims) == 1 && length(sdims) == 1)  {    # 1D array quick bypass
      return(as.numeric(response$value))
    }

    if (length(rdims) == 2 && length(sdims) == 2)  {    # 2D array quick bypass
      if (transfermode == 'JSON')  {
        result <- response$value 
        A <- matrix(nrow = rdims[1], ncol = rdims[2])
        for (i in 1:rdims[1])  {
          A[i,] <- as.numeric(result[[i]])
        }
        return(A)
      } else if (transfermode == 'binary')  {
        result <- extractBinary(dataset@type, prod(rdims), response)
        A <- matrix(data=result, nrow = rdims[2], ncol = rdims[1])
        return(t(A))
      }
    }

    nn <- prod(rdims)
    A <- array(rep(0, nn))
    if (transfermode == 'JSON')  {
      # unpack response into an R array 
      result <- response$value

      # multi-dimensional data is returned as a list.
      if (is.list(result))  {
        A[1:nn] <- .Call("extractJSON", as.integer(length(sdims)), as.integer(sdims), result)
      } else  {  # one-dimensional data is returned as a vector 
        A[1:nn] <- as.numeric(result)
      }
    }  else if (transfermode == 'binary')  {
      result <- extractBinary(dataset@type, nn, response)
      A[1:nn] <- .Call("extractBin", as.integer(length(sdims)), as.integer(sdims), result)
    }

    # Question: should AA be forced to rdims or sdims?
    # For a slice of a multi-dimensional array, we want
    # to force to the subset. (rdims)

    # But if DelayedArray requires a 1 x N array, we
    # want the returned value to be 1 x N, not a 
    # vector of N. (sdims)

    # Arbitrary choice: force flat (rdims) and let 
    # the calling routine redimension.

    AA <- array(A, dim = rdims)
    AA
}

# private - split numeric vectors into slices and fetch
# data in one or more requests.
getDataList <- function(dataset, indices, transfermode = 'JSON')  {
    if (length(dataset@shape) != length(indices))
      stop("wrong length of indexlist")

    slicelist <- lapply(indices, slicify)
    slclen <- lapply(slicelist, length)

    if (any(unlist(slclen) > 1))  {
      # assemble block arrays 
      AA <- multifetch(slicelist, dataset)
    } else  {
      # simple case: one block
      AA <- getDataVec(dataset, unlist(slicelist), transfermode)
    }
    AA

}

# private - in which we try to anticipate all the invalid things 
# users will try to enter for indices
checkSlices <- function(shape, slices)  {
  ok <- TRUE
  if (length(slices) != length(shape)) {
    message("wrong number of indices")
    ok <- FALSE
  }
  
  slicelist <- vector("list", length(slices))

  for (i in seq_along(slices))  {
    slice <- slices[i]
    start <- -1
    stop <- -1
    step <- -1

    st <- strsplit(slice, ':')[[1]]
    ss <- as.numeric(st)
    ss[which(is.na(ss))] <- -1

    if (slice == ':')  {
      start <- 1
      stop <- shape[i]
      step <- 1
    } else {
      if (length(ss) == 1)  {    # this is a slice like '5:' 
        start <- ss[1]
        stop <- shape[i]
        step <- 1
      } else if (length(ss) == 2)  {
        if (st[1] == '')  {      # ':5'
          start <- 1 
          stop <- ss[2]
          step <- 1
        } else  {
          start <- ss[1]
          stop <- ss[2]
          step <- 1
        }
      } else if (length(ss) == 3)  {
          start <- ss[1]
          stop <- ss[2]
          step <- ss[3]
      } else  {
        message(paste0("malformed slice ", i))
        ok <- FALSE
      }
    }

    slicevec <- c(start, stop, step)
    if (any(slicevec < 0))  {
      message(paste0("malformed slice ", i))
      ok <- FALSE
    }
    if (any(slicevec != trunc(slicevec)))  {
      message(paste0("malformed slice ", i))
      ok <- FALSE
    }

    slicelist[[i]] <- slicevec

    if (0 >= start || start > shape[i])  {
      message(paste0("slice start out of range in slice ", i))
      ok <- FALSE
    }
    if (0 >= stop || stop > shape[i])  {
      message(paste0("slice stop out of range in slice ", i))
      ok <- FALSE
    }
    if (stop < start)  {
      message(paste0("slice stop less than slice start in slice ", i))
      ok <- FALSE
    }
  }

  if (!ok)
    return(list())

  strslices <- lapply(slicelist, 
    function(slc) { sprintf('%d:%d:%d', slc[1], slc[2], slc[3]) })
  pyslices <- lapply(strslices, r2pyslice) 

}

# private - convert R-slice (string) to python-slice (string)
r2pyslice <- function(slcstr)  {
  slc <- as.numeric(strsplit(slcstr, ':')[[1]])
  # python indices from 0 instead of 1
  slc[1] <- slc[1]-1
  slc[2] <- slc[2]-1
  # if (stop-start) % step == 0, python slice excludes stop
  if ( (slc[2]-slc[1]) %% slc[3] == 0 )
    slc[2] <- slc[2] + 1
  slcstr <- sprintf('%d:%d:%d', slc[1], slc[2], slc[3])
}

# private - convert R-slice (vec) to python-slice (vec)
r2pyslcvec <- function(slc)  {
  # python indices from 0 instead of 1
  slc[1] <- slc[1]-1
  slc[2] <- slc[2]-1
  # if (stop-start) % step == 0, python slice excludes stop
  if ( (slc[2]-slc[1]) %% slc[3] == 0 )
    slc[2] <- slc[2] + 1
  slc
}

# Note: for more than two dimensions, "column-major" means
# "first-fastest" and "row-major" means "last-fastest"

# private - extract column-major subscripts for linear index
csub4idx <- function(D, ind)  {
  m <- length(D)
  X <- rep(-1, m)
  off <- ind-1
  
  X <- vapply(0:(m-1), function(j)  {
    if (j == 0)  {
      s <- off %% D[1]      
    } else if (j == m-1)  {
      s <- off %/% prod(D[1:m-1])
    } else  {
      s <- (off %/% prod(D[1:j])) %% D[j+1]
    } 
    s
  }, numeric(1))
  X <- X+1
  X
}

# private - extract column-major subscripts for linear index
rsub4idx <- function(D, ind)  {
  m <- length(D)
  X <- rep(-1, m)
  off <- ind-1
  
  X <- vapply(0:(m-1), function(j)  {
    if (j == 0)  {
      s <- off %/% prod(D[2:m])
    } else if (j == (m-1))  {
      s <- off %% D[m]
    } else  {
      s <- (off %/% prod(D[(j+2):m])) %% D[j+1]
    }
    s
  }, numeric(1))
  X <- X+1
  X
}

# private - extract column-major linear index for subscripts
cidx4sub <- function(D, S)  {
  m <- length(D)
  S <- S-1
  off <- 0
  for (j in 0:(m-1))  {
    p <- ifelse(j == 0, 1, prod(D[1:j]))
    off <- off + S[j+1]*p
  }
  off+1
}

# private - extract row-major linear index for subscripts
ridx4sub <- function(D, S)  {
  m <- length(D)
  S <- S-1
  off <- 0
  for (j in 0:(m-1))  {
    p <- ifelse(j == m-1, 1, prod(D[(j+2):m]))
    off <- off + S[j+1]*p
  }
  off+1
}



# private - length of valid (Python) slice
slicelen <- function(slc)  {
  ss <- as.numeric(strsplit(slc, ':')[[1]])
  sdim <- (ss[2]-ss[1]) %/% ss[3]
  if ((ss[2]-ss[1]) %% ss[3] != 0)
    sdim <- sdim + 1
  sdim
}


# private - extract binary data from response
# reference: https://support.hdfgroup.org/HDF5/doc1.8/RM/PredefDTypes.html
#' @importFrom utils strcapture
extractBinary <- function(typ, nele, rsp)  {

  # standard defaults
  what <- 'integer'
  size <- NA_integer_
  signed <- TRUE
  endian <- .Platform$endian     # just a guess - the server determines the endianness

  df <- strcapture('H5T_([[:alnum:]]*)_([IFUBD])([[:digit:]]+)([LB]E)', typ$base, 
                   data.frame(cl=character(), wh=character(), sz=integer(), 
                              en=character(), stringsAsFactors=FALSE))
  if (nrow(df) != 1) 
    stop(paste0("binary transfer for type ", typ$base, " not implemented yet"))
  if (!(df[1,1] %in% c('STD', 'IEEE')))
    stop(paste0("binary transfer for type ", typ$base, " not implemented yet"))

  if (df[1,2] == 'I' && df[1,3] == '32')  {
    what <- 'integer'
    size <- 4
  } else if (df[1,2] == 'I' && df[1,3] == '64')  {
    what <- 'integer'
    size <- 8
  } else if (df[1,2] == 'F' && df[1,3] == '64')  {
    what <- 'double'
    size <- 8
  } else  {
    stop(paste0("binary transfer for type ", typ$base, " not implemented yet"))
  }
  endian <- ifelse(df[1,4] == 'LE', 'little', 'big')
  result <- readBin(rsp$content, what=what, n=nele, size=size, signed=signed, endian=endian)
  result

}

# slicify - convert an arbitrary vector into slices
# This is an unsightly kludge, but it is designed to ensure
# that there is no more than one slice of width one.
# All other singletons should end up squashed into 
# pairs with each other. The idea is that the time 
# required to execute the loop is dwarfed by the time
# required to execute an extra remote fetch.
slicelst <- function(v)  {

  ll <- vector("list", length = length(v))   
  vec <- rep(0, length(v))
  
  il <- 1
  if (length(v) <= 2)  {
    ll <- list(v)
  } else {
  
    vec[1] <- v[1]
    vec[2] <- v[2]
    nv <- 2
    i <- 3
    while (i <= length(v))  {
      if (v[i]-v[i-1] == v[i-1]-v[i-2])  {
        nv <- nv + 1
        vec[nv] <- v[i]
        i = i + 1
      } else  {
        ll[[il]] <- vec[1:nv]
        il <- il + 1
        if (i < length(v))  {
          vec[1] <- v[i]
          vec[2] <- v[i+1]
          nv <- 2
          i = i + 2
        } else {
          vec[1] <- v[i]
          nv <- 1
          i = i + 1
        }
     }
    }
    if (nv > 0)  {
      ll[[il]] <- vec[1:nv]
    }
    ll <- ll[-which(sapply(ll, is.null))]
  }
  ll
}
  
# make list of vecs into string slices
slicify <- function(v)  {
  ll <- slicelst(v)
  slices <- vapply(ll, function(vec) { 
    start <- vec[1] 
    stop <- vec[length(vec)] 
    step <- ifelse(length(vec) == 1, 1, vec[2]-vec[1])
    sprintf("%d:%d:%d", start, stop, step)
  }, character(1))
}

# private - fetch and assemble dataset blocks 
multifetch <- function(LL, dataset)  { 
  slicelen <- function(slc)  {
    ss <- as.numeric(strsplit(slc, ':')[[1]])
    r <- (ss[2]-ss[1]) %/% ss[3]
    r+1
  }

  # MM[[d]][[i]] is the length of the ith slice in dimension d
  MM <- lapply(LL, function(L) lapply(L, function(slc) slicelen(slc)))

  # N[d] is the length of the result array in dimension d 
  N <- unlist(lapply(seq_along(MM), function(m) sum(unlist(MM[[m]]))))

  # B[d] is the number of slices in dimension d
  B <- unlist(lapply(seq_along(MM), function(m) length(unlist(MM[[m]]))))

  # pre-allocate result array
  R <- array(rep(0,prod(N)), dim=N) 

  # the total number of fetches is the product of the numbers of slices
  nf <- prod(B)

  # loop over fetches (this can be vapply later)
  for (i in 1:nf)  {

    # select ith block to fetch
    sbs <- rsub4idx(B, i)
    scs <- lapply(seq_along(sbs), function(j) LL[[j]][[sbs[j]]])
  
    # fetch block
    blk <- .getData(dataset, unlist(scs))   # is scs right?

    # put into correct subarray of R 
    nd <- length(LL)
    arglst <- vector(mode="list", length = nd)
    for(d in 1:nd)  {
      umm <- unlist(MM[[d]])
      startpos <- 1
      if (sbs[d] != 1)  {
        startpos <- 1+sum(umm[1:(sbs[d]-1)])
      }
      length <- MM[[d]][[sbs[d]]]
      arglst[[d]] <- seq(startpos, startpos+length-1)
    }  
    R <- do.call('[<-', c(list(R), arglst, list(blk)))
  }
  
  # squash flat dimensions out

  # note: This is kind of a kludge, it would be better to
  # figure out the final dimensions ahead of time and 
  # modify the for loop with conditionals. But if this 
  # works, it evades unnecessary code complexity.

  NN <- N[which(N != 1)]
  R <- array(R, dim=NN)

  R
}

setMethod("show", "HSDSDataset", function(object) {
 cat(paste("rhdf5client HSDSDataset instance, with shape "))
 dput(object@shape)
 cat("  use getData(...) or square brackets to retrieve content.\n")
})


#' bracket method for 1d request from HSDSDataset
#' @param x object of type HSDSDataset
#' @param i vector of indices (first dimension)
#' @param j not used
#' @param \dots not used
#' @param drop logical(1) if TRUE return has no array character
#' @return an array with the elements requested from the HSDSDataset
#' @docType methods
# special case: one-dimensional arrays
setMethod('[', c("HSDSDataset", "numeric", "ANY", "ANY"), 
  function(x, i, j, ..., drop) {
    #.Deprecated("HSDSArray", NULL, deprecate_msg)
    getDataList(x, list(i), transfermode='JSON')
  })

# special case: two-dimensional arrays
#' bracket method for 2d request from HSDSDataset
#' @param x object of type HSDSDataset
#' @param i vector of indices (first dimension)
#' @param j vector of indices (second dimension)
#' @param \dots not used
#' @param drop logical(1) if TRUE return has no array character
#' @return an array with the elements requested from the HSDSDataset
#' @docType methods
setMethod('[', c("HSDSDataset", "numeric", "numeric", "ANY"), 
  function(x, i, j, ..., drop) {
    #.Deprecated("HSDSArray", NULL, deprecate_msg)
    getDataList(x, list(i, j), transfermode='JSON')
  })


#Undocumented S4 methods:
#  generic '[' and siglist 'HSDSDataset,numeric,ANY,ANY'
#  generic '[' and siglist 'HSDSDataset,numeric,numeric,ANY'

