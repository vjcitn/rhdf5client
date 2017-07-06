#' Utility function for retrieving matrices from a two-dimensional dataset
#' 
#' @param dset A two-dimensional dataset of type H5S_dataset 
#' @param i A vector of row indices to retrieve
#' @param j A vector of column indices to retrieve
#'
#' @return The submatrix defined by the indices from the dataset 
#'
#' @export

submatrix <- function(dset, i, j)  {
  ind1 = sproc(isplit(i))  # may need to be double loop
  ind2 = sproc(isplit(j))
  if (length(ind1)==1 & length(ind2)==1) 
    ans = dset[ ind1[[1]], ind2[[1]] ]
  else if (length(ind2)==1) {
    cidx <- ind2[[1]]
    column.block.list <- lapply(ind1, function(ridx)  {
      mm <- dset[ridx, cidx]
    })
    ans <- do.call(rbind, column.block.list)
  }
  else if (length(ind1)==1) {
    ridx <- ind1[[1]]
    row.block.list <- lapply(ind2, function(cidx)  {
      mm <- dset[ridx, cidx]
    })
    ans <- do.call(cbind, row.block.list)
  }
  else {
    column.block.list <- lapply(ind1, function(ridx) {
       row.block.list <- lapply(ind2, function(cidx) {
         mm <- dset[ridx, cidx]
       })
       do.call(cbind, row.block.list)
    })
    ans = do.call(rbind, column.block.list)
  }
  ans
}
