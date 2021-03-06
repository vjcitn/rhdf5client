% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RHDF5Array.R
\name{extract_array}
\alias{extract_array}
\alias{extract_array,HSDSArraySeed-method}
\title{Access dataset backed by an HSDSArraySeed}
\usage{
\S4method{extract_array}{HSDSArraySeed}(x, index)
}
\arguments{
\item{x}{An object of type HSDSArraySeed}

\item{index}{A list of numeric vectors to be accessed, one vector 
for each dimension of the array object. A NULL vector indicates
the entire range of indices in that dimension. A zero-length
vector indicates no indices in the relevant dimension. (Accordingly,
any zero-length vector of indices will result in an empty array
being returned.)}
}
\value{
An array containing the data elements corresponding to the 
indices requested
}
\description{
Access dataset backed by an HSDSArraySeed
}
