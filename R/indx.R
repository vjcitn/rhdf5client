
# utilities for index processing
# sproc(isplit(vec)) will convert vec representing R integer vector
# into a list of HDF5server 'select' index candidates
#' isplit converts a numeric vector into a list of sequences for compact reexpression
#' @name isplit
#' @rdname sproc
#' @import methods
#' @param x a numeric vector (should be integers)
#' @return list of vectors of integers which can be expressed as initial/final/stride triplets 
#' @export
isplit = function(x) {
 if (length(x)==1) return(list(`1`=x))
 dx = diff(x)
 rdx = rle(dx)
 if (all(rdx$lengths==1)) return(split(x,x)[as.character(x)])
 grps = c(1, rep(1:length(rdx$length), rdx$length))
 split(x, grps)
}

#' sproc massages output of isplit into HDF5 select candidates
#' @name sproc
#' @rdname sproc
#' @param spl output of isplit
#' @return list of colon-delimited strings each with initial/final/stride triplet 
#' @note Very preliminary implementation.
#' @examples
#' inds = c(1:10, seq(25,50,2), seq(200,150,-2))
#' sproc(isplit(inds))
#' @export
sproc = function(spl) {
# spl is output of isplit
  ans = lapply(spl, function(x) {
    if (length(x)==1) return(paste(x-1,":",x,":1", sep=""))
    d = x[2]-x[1]
    return(paste(x[1]-1, ":", x[length(x)], ":", as.integer(d),
      sep=""))
  })
  ans
}

