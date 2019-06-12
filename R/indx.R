deprecate_msg = paste0("This function is deprecated. The new interface to rhdf5client",
 " is exclusively through its DelayedArray backend HSDSArray")

# utilities for index processing
#'
#' This function is deprecated and will be defunct in the next release.
#'
# sproc(isplit(vec)) will convert vec representing R integer vector
# into a list of HDF5server 'select' index candidates
#' isplit converts a numeric vector into a list of sequences for compact reexpression
#' @name isplit
#' @rdname sproc
#' @import methods
#' @param x a numeric vector (should be integers)
#' @return list of vectors of integers which can be expressed as initial/final/stride triplets 
#' @export
isplit = function(x)  {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  if (length(x)==1) return(list(`1`=x))
  y <- rep(0,length(x))
  i <- 3
  while (i <= length(x))  {
    if (x[i]-x[i-1] == x[i-1]-x[i-2])  { 
      i <- i + 1
    } else  {
      y[i] <- 1
      i <- i + 2
    }
  }
  grps <- cumsum(y)+1
  split(x, grps)
}

nosci <- function(x) format(x, scientific=FALSE)

#' sproc makes vector of type character of triplets initial:final:stride in R-conventions
#' @name sproc
#' @rdname sproc
#' @param spl output of isplit
#' @return list of colon-delimited strings each with initial/final/stride triplet 
#' @examples
#' inds = c(1:10, seq(25,50,2), seq(200,150,-2))
#' sproc(isplit(inds))
#' @export
sproc = function(spl)  {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  ans <- lapply(spl, function(x) {
    if (length(x) == 1) return(paste(nosci(x), ":", nosci(x), ":1", sep=""))
    d <- x[2]-x[1]
    paste(nosci(x[1]),":",nosci(x[length(x)]),":",d, sep="")
  })
  ans <- r2py(ans)
  ans
}

# r2py makes R-convention strings into python-convention strings
# HDF5 server uses python conventions
# R conventions: array indices begin at 1, and ranges include last element
# python conventions: array indices begin at 0 and ranges exclude last element
# 
# HDF5 server does not allow decreasing index ranges. Consistency requires 
# the range c(7, 6, 5, 4, 3) should have the same elements as c(3, 4, 5, 6, 7)
# except flipped. r2py maps "3:7:1" to "2:7:1", so it maps "7:3:-1" to 
# "7:2:-1". rhdf5client will flip this to "3:7:1", fetch the data, then flip it back. 
r2py <- function(spr)  {
  ans <- lapply(spr, function(x)  {
    v <- as.numeric(unlist(strsplit(x, ":")))
    if (v[3] > 0)  {
      paste(nosci(v[1]-1), ":", nosci(v[2]), ":", nosci(v[3]), sep="")
    } else  {
      paste(nosci(v[1]), ":", nosci(v[2]-1), ":", nosci(v[3]), sep="")
    }
  })
  ans 
}

# ivindx reverses a vector of negative indices into a vector of positive indices
# example: ii <- ivindx(-c(2, 4, 8), 10)    # c(1, 3, 5, 6, 7, 8, 9)
ivindx <- function(iidx, didx)  {
  ii <- -iidx
  v <- 1:didx
  v[ !(v %in% ii) ]
}









