deprecate_msg = paste0("This function is deprecated. The new interface to rhdf5client",
 " is exclusively through its DelayedArray backend HSDSArray")

#' manage h5serv URL
#'
#' This function is deprecated and will be defunct in the next release.
#'
#' @return URL of h5serv server
#' @examples
#' URL_h5serv()
#' @export
URL_h5serv = function() { 
#  .Deprecated("HSDSArray", NULL, deprecate_msg)
  "http://h5s.channingremotedata.org:5000"
}

#' manage hsds URL
#'
#'
#' @return URL of hsds server
#' @examples
#' URL_hsds()
#' @export
URL_hsds = function() {
  "http://hsdshdflab.hdfgroup.org"
}

#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom rjson fromJSON
#' @importFrom utils capture.output
.serverURL = function(x) x@serverURL


#' H5S_source identifies an HDF5/HSDS server and manages some metadata about contents
#'
#' This class is deprecated and will be defunct in the next release.
#'
#' @name H5S_source
#' @rdname H5S_source-class
#' @slot serverURL character string with a URL
#' @slot dsmeta DataFrame instance with metadata about content of h5serv server
#' @slot dmains DataFrame instance with metadata about the content of hsds server
#' @slot getReq DataFrame instance with metadata about hsds server
#' @slot folderPath character string with path to user's folder/file on hsds server
#' @aliases H5S_source-class
#' @exportClass H5S_source

setClass("H5S_source", representation(serverURL="character", dsmeta="DataFrame", getReq = "DataFrame",dmains= "DataFrame", folderPath = "character"))
setMethod("show", "H5S_source", function(object) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  if(nrow(object@dsmeta)>1){
  cat(" H5serv server url : ", object@serverURL, 
      "\n There are", nrow(object@dsmeta), "groups.",
      "\n Use groups(), links(), ..., to probe and access metadata.",
      "\n Use dsmeta() to get information on datasets within groups.",
      "\n Use [[ [dsname] ]]  to get a reference suitable for [i, j] subsetting.\n")
  }
  else{
  cat(" HSDS server url :", object@serverURL,
      "\n Use getReq() to get information on the server.",
      "\n Use setPath() to specify path to a hdf5 file.",
      "\n Use fetchDatasets() to get id of the dataset of interest.\n")
    }
})

# private: extract hostname from a file URL (return null for group URL)
fixtarget = function(x) sub(".*host=(.*).h5s.channingremotedata.org", "\\1", x)

#' construct H5S_source
#'
#' This function is deprecated and will be defunct in the next release.
#'
#' @name H5S_source
#' @rdname H5S_source-class
#' @param serverURL a URL for a port for HDF5Server
#' @param domain character string with path to file for HSDS 
#' @param \dots not used
#' @note The dsmeta slot holds a DataFrame with a column \code{dsnames}
#' that is a list with ith element a character vector of all dsnames
#' available for the ith group.  There is no effort at present to
#' search all groups for candidate datasets.
#' @note If the domain for the HSDS server is known, 
#' pass the domain path as a character string along with ther serverURL
#' @return an initialized object of type H5S_source
#' @examples
#' \dontrun{
#' bigec2 = H5S_source(URL_h5serv()) # h5serv 
#' bigec2
#' dsmeta(bigec2)[1:2,]       # two groups
#' dsmeta(bigec2)[1,2][[1]]   # all dataset candidates in group 1
#' }
#' hsdsCon = H5S_source(URL_hsds()) # hsds server connection
#' hsdsCon
#' getReq(hsdsCon)
#' setPath(hsdsCon,"/home/stvjc/hdf5_mat.h5") -> hsds
#' fetchDatasets(hsds)     # grab the dataset id of interest 
#' H5S_dataset2(hsds, "d-a9e4b71c-8ea2-11e8-9306-0242ac120022")
#' @export
H5S_source = function(serverURL, domain, ...) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  if(missing(domain)){
    serverCheck = serverVersion(serverURL)
    if(serverCheck == 1){
      tmp <- new("H5S_source", serverURL=serverURL, dsmeta=DataFrame())
      grps <- groups(tmp)
      message("analyzing groups for their links...")
      thel <- targs <- List(targs=lapply( seq_len(nrow(grps)), 
                                          function(x) fixtarget(hosts(links(tmp,x)))))
      message("done")
      dsm <- DataFrame(groupnum=seq_len(nrow(grps)), dsnames=thel, grp.uuid=grps$groups)
      obj <- new("H5S_source", serverURL=serverURL, dsmeta=dsm)
      obj
    }
    else{
      tmp <- new("H5S_source", serverURL=serverURL, getReq=DataFrame())
      get <- hsdsInfo(tmp)
      get.df <- DataFrame(get)
      obj <- new("H5S_source", serverURL=serverURL, getReq=get.df )
    }
  }
  else{
    tmp <- new("H5S_source", serverURL=serverURL, getReq=DataFrame())
    get <- hsdsInfo(tmp)
    get.df <- DataFrame(get)
    obj <- new("H5S_source",serverURL=serverURL, getReq=get.df, folderPath=domain)
    #H5S_dataset2(obj)
  }
}

#' list information about datasets available in an H5S_source
#'
#' This function is deprecated and will be defunct in the next release.
#'
#' @param src H5S_source instance
#' @return data frame with one row for each group and three columns. The 
#' second column has the list of datasets in the group.
#' @examples
#' \dontrun{
#' bigec2 = H5S_source(URL_h5serv())
#' dsm <- dsmeta(bigec2) 
#' dst <- unlist(dsm[1,2])    # all dataset candidates in group 1
#' }
#' @export
dsmeta = function(src) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  src@dsmeta
}

#' list information about server content available in an H5S_source hsds instance
#'
#' This function is deprecated and will be defunct in the next release.
#'
#' @param src H5S_source instance
#' @return data frame with 5 columns for one row for each user's data
#' @export
getReq = function(src) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  src@getReq
}

#' Subscript operator
#'
#' This function is deprecated and will be defunct in the next release.
#'
#' @name [[
#' @aliases [[,H5S_source,character-method
#' @param x instance of H5S_source
#' @param i character string intended to identify dataset on server
#' @param j not used
#' @exportMethod '[['
setMethod("[[", c("H5S_source", "character", "ANY"), function(x, i, j) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  dataset(x, i)
})

#' HDF5 server data groups accessor
#' @exportMethod groups
#' @export groups
#' @docType methods
#' @rdname groups-methods
setGeneric("groups", function(object, index, ...) standardGeneric("groups"))

#' @param object H5S_source instance
#'
#' This function is deprecated and will be defunct in the next release.
#'
#' @param index numeric, if present, extracts metadata about selected group (sequential ordering 
#' of groups as returned by server) access for group information for HDF5 server
#' @param \dots not used
#' @return a data frame with group name and number of links for each group
#' @examples
#' \dontrun{
#' bigec2 = H5S_source(URL_h5serv())
#' groups(bigec2)
#' }
#' @rdname groups-methods
#' @aliases groups,H5S_source,missing-method 
setMethod("groups", c("H5S_source", "missing"), function(object, index, ...) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  target = paste0(.serverURL(object), "/groups")
  ans = transl(target) # fromJSON(readBin(GET(target)$content, what="character"))

  # find the root group: hh is a matrix with first column "href" and second "rel"
  # Find the one whose "rel" column is "root" and pull the name out of the "href"

  if (length(ans$hrefs)<1)  { 
    stop("no group table at server URL")
  }
  hh = t(sapply(ans$hrefs, force))     
  rootgroup = sub(".*groups.", "", hh[hh[,2]=="root", 1])  
  grps = c(rootgroup, unlist(ans$groups))   # all groups plus the root group

  # get the link count for each group
  nl = vapply(1:length(grps), function(x) {
    gname = grps[x]
    target = paste0(.serverURL(object), "/groups/", gname, "/links" )
    ans = transl(target)  # fromJSON(readBin(GET(target)$content, what="character"))
    length(ans$links)
  }, integer(1))
  DataFrame(groups=grps, nlinks=nl)
})

#' @aliases groups,H5S_source,numeric-method
#' @rdname groups-methods
setMethod("groups", c("H5S_source", "numeric"), function(object, index, ...) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  groups(object)[index,,drop=FALSE]
})

setClass("H5S_linkset", representation(links="list", group="character",
                                       source="H5S_source"))
setMethod("show", "H5S_linkset", function(object) {
  cat("HDF5 server link set for group", object@group, "\n")
  cat(" There are", length(object@links$links), "links.\n")
  cat(" Use targets([linkset]) to extract target URLs.\n")
})

#'set path for hsds server resource
#'
#' This function is deprecated and will be defunct in the next release.
#'
#'@param object H5S_source instance
#'@param folderPath character string with path to user's folder on hsds server
#'@param \dots not used
#'@return an updated object with folderPath set
#'@examples
#'hsdsCon = H5S_source(URL_hsds()) # hsds server connection
#'setPath(hsdsCon, "/home/stvjc/hdf5_mat.h5")-> hsds
#'@docType methods
#'@rdname setPath
#'@aliases setPath,H5S_source,character-method
#'@export setPath
#'@exportMethod setPath
setGeneric("setPath", function(object,folderPath, ...) standardGeneric("setPath"))
setMethod("setPath", c("H5S_source","character"), function(object, folderPath, ...) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  object@folderPath = folderPath
  object
})

#' access for link metadata for HDF5 server groups
#'
#' This function is deprecated and will be defunct in the next release.
#'
#' @param object H5S_source instance
#' @param index numeric group index
#' @param \dots not used
#' @return an object of type H5S_linkset with the linkset of the group
#' @examples
#' \dontrun{
#' bigec2 = H5S_source(URL_h5serv())
#' lks <- links(bigec2, 1)    # linkset for root group 
#' urls <- targets(lks)       # URLs of datasets in linkset
#' }
#' @rdname links
#' @aliases links,H5S_source,numeric-method
#' @export links
#' @exportMethod links
setGeneric("links", function(object, index, ...) standardGeneric("links"))
setMethod("links", c("H5S_source", "numeric"), function(object, index, ...) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  gname = groups(object, index)[["groups"]][1] # skirt mcols bug
  target = paste0(.serverURL(object), "/groups/", gname, "/links" )
  ans = transl(target) # fromJSON(readBin(GET(target)$content, what="character"))
  new("H5S_linkset", links=ans, source=object, group=gname)
})

.links <- function(linkset) { linkset@links }    # private accessor

#' provide the full URLs for link members
#'
#' This function is deprecated and will be defunct in the next release.
#'
#' @param h5linkset instance of H5S_linkset
#' @param index numeric index into link vector - ignored
#' @return a vector of dataset tags
#' @rdname targets
#' @examples
#' \dontrun{
#' bigec2 = H5S_source(URL_h5serv())
#' lks <- links(bigec2, 1)    # linkset for root group 
#' urls <- targets(lks)       # URLs of datasets in linkset
#' }
#' @export

targets = function(h5linkset, index) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  lks <- .links(h5linkset)
  vapply(h5linkset@links$links, function(lk) lk[["target"]], character(1)) 
}

# private: return all URLs in the linkset that link to datasets
# cleanIP: if TRUE, remove the URL up to the host 
# note: index is ignored in targets(), so ignored in hosts()
hosts = function(h5linkset, index, cleanIP=TRUE) {
  ans = targets(h5linkset, index)      
  ans = ans[grep("host=", ans)]
  if (cleanIP) {
    gsub(".*host=", "host=", ans)
  } else {
    ans
  }
}

#' construct H5S_dataset object
#'
#' This function is deprecated and will be defunct in the next release.
#'
#' @name H5S_dataset
#' @import S4Vectors
#' @slot source instance of H5S_source instance
#' @slot simpleName character string naming dataset 
#' @slot shapes list including dimension information
#' @slot hrefs DataFrame of hrefs as defined in the API
#' @slot allatts list of all attributes
#' @slot presel string prepared for select operation in GET
#' @slot transfermode default "JSON" or "binary" for binary transfer
#' @rdname H5S_dataset
#' @aliases H5S_dataset-class
#' @exportClass H5S_dataset
setClass("H5S_dataset", representation(
  source="H5S_source", simpleName="character",
  shapes="list", hrefs="DataFrame", allatts="list", 
  presel="character", transfermode="character"))
setMethod("show", "H5S_dataset", function(object) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  cat("H5S_dataset instance:\n")
  curdim = object@shapes$dims
  print(data.frame(dsname=object@simpleName, intl.dim1=curdim[1], intl.dim2=curdim[2], 
                   created=object@allatts$created, type.base=object@allatts$type$base))
  #cat("Use [[", object@simplename, "]] to acquire reference amenable to [i,j] subsetting.\n")
})

#' replace transfer mode
#'
#' This function is deprecated and will be defunct in the next release.
#'
#' @name transfermode<-
#' @param object instance of H5S_linkset
#' @param value either "JSON" (default) or "binary"
#' @return updated object of type H5S_dataset
#' @rdname transfermode
#' @aliases transfermode<-,H5S_dataset-method
#' @docType methods
#' @export
setGeneric("transfermode<-", def = function(object, value) { standardGeneric("transfermode<-") })
setReplaceMethod("transfermode", "H5S_dataset", 
  function(object, value) {  
    #.Deprecated("HSDSArray", NULL, deprecate_msg)
    if ( value == "JSON" | value == "binary" )  {
      object@transfermode <- value  
    }  else  {
      warning(paste("ignoring request for unknown transfer mode ", value))
    }
    object
  }
)


# private: get content from host
# param targ is the URL with query
transl = function(targ)  {  
  rsp <- GET(targ)
  if (rsp$status_code != 200)  
    stop(paste("error: can't read JSON ", targ, sep=""))
  jsn <- readBin(rsp$content, what="character")
  fromJSON(jsn)
}

# private: get numeric content from host by binary transfer
# param targ is the URL with query
# param nele is the number of numeric elements expected
bintransl = function(targ, nele, h5typ)  {  
  rsp <- GET(targ, add_headers(Accept="application/octet-stream"))
  if (rsp$status_code != 200)  
    stop(paste("error: can't read binary ", targ, sep=""))

  if (h5typ == "H5T_IEEE_I32LE")  {
    readBin(rsp$content, what="integer", n = nele, size = NA_integer_, 
      signed = TRUE, endian = "little")
  } else if (h5typ == "H5T_IEEE_F64LE")  {
    readBin(rsp$content, what="double", n = nele, size = 8, 
      signed = TRUE, endian = "little")
  } else  {
    stop(paste0("error: unrecognized data type in binary transfer: ", h5typ))
  }
}

# private: get numeric content from host by JSON transfer
# param targ is the URL with query
# param nele is the number of numeric elements expected (ignored)
jsontransl = function(uu, nele)  {  
  val <- transl(uu)$value
  if (is.list(val))  {
    result <- do.call(c, val)
  } else {
    result <- val
  }
}


#private : to check if the server being called is hsds or h5serv
serverVersion <- function(serverURL = serverURL){
  #validURL = url.exists(serverURL)
  if(httr::http_status(GET(serverURL))$reason == "OK"){
    serverResponse = fromJSON(file=serverURL)
    if("root" %in% attributes(serverResponse)$names){
      flag = 1   ### This is a h5serv
      return(flag)
    }
    else{ 
      flag = 2   ### This is a hsds server
      return(flag)
    }
  }
  else{
    flag = 2
    return(flag) # this is the new hsds server
  }
  
}

#' extract elements from H5S_dataset
#'
#' This function is deprecated and will be defunct in the next release.
#'
#' @param x instance of H5S_dataset
#' @param i select option for first matrix index in HDF5 server value API
#' @param j select option for second matrix index in HDF5 server value API
#' @param \dots unused
#' @param drop logical defaults to FALSE
#' @return matrix of data obtained
#' @exportMethod [
setMethod("[", c("H5S_dataset", "numeric", "numeric", "ANY"), function(x, i, j, ..., drop=FALSE) {
# bracket selection passed directly to HDF5 server ... row-major
#
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  ii <- as.integer(i)
  jj <- as.integer(j)
  if (any(ii != i) | any(jj != j))  {
    stop("index is non-integral")
  }

  if (all(i < 0)) {
    i <- ivindx(i, internalDim(x)[1]) 
  }
  if (all(j < 0)) {
    j <- ivindx(j, internalDim(x)[2]) 
  }
  if (any(i <= 0) | any(j <= 0) )  {
    stop("index is negative")
  }
  if (any(i > internalDim(x)[1]) | any(j > internalDim(x)[2]))  {
    stop("index is negative")
  }

  ind1 <- sproc(isplit(i))
  ind2 <- sproc(isplit(j))

  if (length(ind1) == 1 & length(ind2) == 1)  {
    ans <- t(x[ ind1[[1]], ind2[[1]] ])
  } else if (length(ind2) == 1)  {
    ansl <- lapply(ind1, function(i1) t(x[i1, ind2[[1]]]))
    ans <- do.call(cbind, ansl)
  } else if (length(ind1) == 1)  {
    ansl <- lapply(ind2, function(i2) t(x[ind1[[1]], i2]))
    ans <- do.call(rbind, ansl)
  } else  {
    ansl <- lapply(ind1, function(i1)  {
      do.call(rbind, lapply(ind2, function(i2)  {
        t(x[i1, i2])
      }))
    })
    ans <- do.call(cbind, ansl)
  }
  t(ans)
})
  

#' extract elements from H5S_dataset
#' @aliases [,H5S_dataset,character,character-method
#' @param x instance of H5S_dataset
#' @param i character vector of row selections
#' @param j character vector of column selections
#' @param \dots not used
#' @param drop logical(1) set TRUE to drop array character 
setMethod("[", c("H5S_dataset", "character", "character", "ANY"), function(x, i, j, ..., drop=FALSE) {

  uu = x@presel
  dims = x@shapes$dims

  ind1lims = as.numeric(strsplit(i, ":")[[1]])
  if (ind1lims[1] < 0) 
    stop("negative starting index not allowed in i")
  if (ind1lims[2] > dims[1]) 
    stop("i exceeds boundary for first index")

  ind2lims = as.numeric(strsplit(j, ":")[[1]])
  if (ind2lims[1] < 0) 
    stop("negative starting index not allowed in j")
  if (ind2lims[2] > dims[2]) 
    stop("j exceeds boundary for second index")

  delta1 <- 1
  if ( length(ind1lims) >= 3) {
    delta1 <- ind1lims[3]
  }
  nrow <- ceiling((ind1lims[2] - ind1lims[1])/delta1)  

  delta2 <- 1
  if ( length(ind2lims) >= 3) {
    delta2 <- ind2lims[3]
  }
  ncol <- ceiling((ind2lims[2] - ind2lims[1])/delta2)  

  # flip negative ranges because HDF5 doesn't allow them
  if ( delta1 < 0 )  {
    # TODO: leave delta1 < 0 for flag
    i <- paste0(ind1lims[2], ":", ind1lims[1], ":", -delta1)
  }
  if ( delta2 < 0 )  {
    # TODO: leave delta2 < 0 for flag
    j <- paste0(ind2lims[2], ":", ind2lims[1], ":", -delta2)
  }
  nele <- nrow*ncol    
  uu = sub("%%SEL1%%", i, uu)
  uu = sub("%%SEL2%%", j, uu)
  h5typ <- x@allatts$type$base
  if ( ( h5typ == "H5T_IEEE_I32LE" | h5typ == "H5T_IEEE_F64LE" ) & 
         x@transfermode == "binary" )  {
    # message(paste("binary transfer", sep=""))
    val <- bintransl(uu, nele, h5typ)
  }
  else  {
    # message(paste("JSON transfer", sep=""))
    val <- jsontransl(uu, nele)
  }
  mat <- matrix(val, nrow=nrow, ncol=ncol, byrow = TRUE, dimnames = NULL)
  # flip back negative ranges
  if (delta1 < 0)  {
    mat <- mat[c(nrow:1),,drop=FALSE]
  }
  if (delta2 < 0)  {
    mat <- mat[,c(ncol:1),drop=FALSE]
  }
  mat
})

#' Find a dataset on source from its name
#'
#' This function is deprecated and will be defunct in the next release.
#'
#' @name dataset
#' @param h5s instance of H5S_source
#' @param tag character string identifying a dataset
#' @return object of type H5S_dataset
#' @export
dataset = function(h5s, tag) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  dsns = dsmeta(h5s)[["dsnames"]] # mcols problem with [,"dsnames"]
  # find row of dsmeta DataFrame where tag is a substring of a dataset name
  # to allow substrings to be used for querying (e.g., "100k" for "neurons100k")
  hits = vapply(dsns, function(x) length(grep(tag, x))>0, logical(1))
  if (!any(hits)) 
    stop("tag not found in dsmeta(h5s)")
  if (sum(hits)>1) 
    warning("tag occurs in several groups, using first")
  fulldsn = dsns[[which(hits)]] #[[1]] # unlist; which(hits) is relevant group
  fulldsn = fulldsn[ grep(tag, fulldsn) ] # find the actual simple name matching substring in [[]]
  lin = links(h5s, which(hits))
  targs = targets(lin)
  targs = targs[grep(tag,targs)]

  if (length(targs)>1)  {
    cat("dataset tag does not identify a unique target. Found:", "\n")
    cat(targs, sep="\n")
    stop("please supply tag to identify single target.")
  }

  targ = sub(".host", "datasets?host", targs)
  uuid = transl(targ)$datasets # fromJSON(readBin(GET(targ)$content, what="character"))$datasets
  attrs = transl( sub("datasets", paste0("datasets/", uuid), targ ) )
  hrnm = vapply(attrs$hrefs, "[[", character(1), 2)
  hrval = vapply(attrs$hrefs, "[[", character(1), 1)
  ans = DataFrame(hrefName=hrnm, hrefValue=hrval)
  rownames(ans) = hrnm
  self = ans["self", "hrefValue"]
  prep = sub("\\?host=", "/value?host=", self)
  prep = paste0(prep, "&select=[%%SEL1%%,%%SEL2%%]")
 
  xjson = "JSON"
  new("H5S_dataset", source=h5s, simpleName=fulldsn,
    shapes=attrs$shape, hrefs=ans, allatts=attrs, presel=prep, transfermode=xjson)
}

#' acquire internal HDF5 dimension information for matrix
#'
#' This function is deprecated and will be defunct in the next release.
#'
#' @param h5d instance of H5S_dataset
#' @return vector with dimensions of dataset
#' @examples
#' \dontrun{
#' bigec2 = H5S_source(URL_h5serv())
#' tex <- bigec2[["tenx_100k_sorted"]]
#' internalDim(tex)
#' }
#' @export
internalDim = function(h5d) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  d = slot(h5d, "shapes")$dims
  c(intl.dim1=d[1], intl.dim2=d[2])
}


#' HSDS server get request accessor
#'
#' This function is deprecated and will be defunct in the next release.
#'
#' @param object H5S_source instance
#' @return a data frame with response
#' @examples 
#' hsdsCon = H5S_source(URL_hsds()) # hsds server connection
#' hsdsInfo(hsdsCon)
#' @rdname hsdsInfo
#' @aliases hsdsInfo,H5S_source-method
#' @docType methods
#' @export hsdsInfo
#' @exportMethod hsdsInfo
setGeneric("hsdsInfo", function(object) standardGeneric("hsdsInfo"))
setMethod("hsdsInfo", c("H5S_source"), function(object) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  #target = paste0(.serverURL(object))
  target = paste0(.serverURL(object),"/domains")
  ans = transl(target) # fromJSON(readBin(GET(target)$content, what="character"))
  
  if (length(ans$domains)<1)  { 
    stop("no domains at server URL")
  }
  hh = t(sapply(ans$domains, force)) 
  DataFrame(hh)
  #domains in the home folder
  #target = paste0(.serverURL(object), "/domains?domain=/home/")
  #ans = transl(target)
  #hd = t(sapply(ans$domains, force))
  #DataFrame(hd)
  
})

#' HSDS server domains accessor
#'
#' This function is deprecated and will be defunct in the next release.
#'
#' @param object H5S_source instance
#' @param \dots not used
#' @return a data frame with domains name
#' @examples 
#' hsdsCon = H5S_source(URL_hsds()) # hsds server connection
#' setPath(hsdsCon, "/home/stvjc/")-> hsds
#' domains(hsds)
#' @rdname domains
#' @aliases domains,H5S_source-method
#' @docType methods
#' @export domains
#' @exportMethod domains
setGeneric("domains", function(object, ...) standardGeneric("domains"))
setMethod("domains", c("H5S_source"), function(object, ...) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  target = paste0(.serverURL(object, ...),"/domains?domain=", object@folderPath)
  #target = paste0(.serverURL(object))
  ans = transl(target) # fromJSON(readBin(GET(target)$content, what="character"))
  
  if (length(ans$domains)<1)  { 
    stop("no domains at server URL")
  }
  hh = t(sapply(ans$domains, force)) 
  DataFrame(hh)
  #domains in the home folder
  #target = paste0(.serverURL(object), "/domains?domain=/home/")
  #ans = transl(target)
  #hd = t(sapply(ans$domains, force))
  #DataFrame(hd)
  
})

#'getDatasetUUIDs from hsds server
#'
#' This function is deprecated and will be defunct in the next release.
#'
#'@param object instance of H5S_source(updated object with path to file set)
#'@return character of dataset uuid obtained 
#'@examples
#'hsdsCon = H5S_source(URL_hsds()) # hsds server
#'setPath(hsdsCon, "/home/stvjc/hdf5_mat.h5")-> hsds
#'getDatasetUUIDs(hsds)
#'@rdname getDatasetUUIDs
#'@export
getDatasetUUIDs <- function(object) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  query = sprintf("%s/datasets?host=%s", object@serverURL, object@folderPath)
  ans = try(GET(query))
  if (inherits(ans, "try-error")) stop("could not resolve datasets query")
  cont = fromJSON(readBin( ans$content, what="character"))
  cont$datasets
}

#'getDatasetAttrs from hsds server
#'
#' This function is deprecated and will be defunct in the next release.
#'
#'@param object instance of H5S_source(updated object with path to file set)
#'@param duid character string with dataset uuid
#'@return list of data obtained
#'@examples
#'hsdsCon = H5S_source(URL_hsds()) # hsds server
#'hsdsCon@folderPath="/home/stvjc/hdf5_mat.h5"
#'ds = fetchDatasets(hsdsCon)# Pick the ID of the dataset you are interested in
#'getDatasetAttrs(hsdsCon, "d-a9e4b71c-8ea2-11e8-9306-0242ac120022")
#'@rdname getDatasetAttrs
#'@export
getDatasetAttrs <- function(object, duid) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  #uu = getDatasetUUIDs(object)
  uu = duid
  query = sprintf("%s/datasets/%s?host=%s", object@serverURL, uu, object@folderPath)
  ans = try(GET(query))   ## is this going to do GET request only for one url? what if the there are multiple datasets in the file?
  if (inherits(ans, "try-error")) stop("could not resolve datasets query")
  cont = fromJSON(readBin( ans$content, what="character"))
  cont
}

#'getDims from hsds server
#'
#' This function is deprecated and will be defunct in the next release.
#'
#'@param object instance of H5S_source(updated object with path to file set)
#'@param duid character string with dataset uuid
#'@return numeric content of dimensions
#'@examples
#'hsdsCon = H5S_source(URL_hsds()) # hsds server
#'setPath(hsdsCon, "/home/stvjc/hdf5_mat.h5")-> hsds
#'duid <- 'd-a9e4b71c-8ea2-11e8-9306-0242ac120022'
#'getDims(hsds, duid)
#'@rdname getDims
#'@export
getDims <- function(object, duid) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  stopifnot(is(object, "H5S_source"))
  getDatasetAttrs(object, duid)$shape$dims
}

#'getHRDF from hsds server
#'
#' This function is deprecated and will be defunct in the next release.
#'
#'@param object instance of H5S_source(updated object with path to file set)
#'@param duid character string with dataset uuid
#'@return DataFrame of data obtained
#'@examples
#'hsdsCon = H5S_source(URL_hsds()) # hsds server
#'hsdsCon@folderPath="/home/stvjc/hdf5_mat.h5"
#'ds = fetchDatasets(hsdsCon) #Pick the ID of the dataset you are interested in
#'getHRDF(hsdsCon, "d-a9e4b71c-8ea2-11e8-9306-0242ac120022")
#'@rdname getHRDF
#'@export
getHRDF <- function(object, duid) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  stopifnot(is(object, "H5S_source"))
  atts = getDatasetAttrs(object, duid)
  nms = sapply(atts$hrefs, "[[", "rel")
  vals = sapply(atts$hrefs, "[[", "href")
  DataFrame(hrefName=nms, hrefValue=vals)
}

#'H5S_dataset2 for datasets in hsds server
#'
#' This function is deprecated and will be defunct in the next release.
#'
#'@param object instance of H5S_source(updated object with path to file set)
#'@param duid character vector with dataset uuid of interest
#'@return H5S_dataset object
#'@examples
#'hsdsCon = H5S_source(URL_hsds()) # hsds server
#'hsdsCon@folderPath="/home/stvjc/hdf5_mat.h5"
#'ds = fetchDatasets(hsdsCon) #Pick the dataset id of interest
#'H5S_dataset2(hsdsCon, "d-a9e4b71c-8ea2-11e8-9306-0242ac120022")
#'@rdname H5S_dataset2
#'@export
H5S_dataset2 = function(object, duid) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  src = new("H5S_source", serverURL=object@serverURL, dsmeta=DataFrame())
  atts = getDatasetAttrs(object, duid)
  ans = getHRDF(object, duid)
  rownames(ans) = ans[,1]
  self = ans["self", "hrefValue"]
  prep = sub("\\?host=", "/value?host=", self)
  prep = paste0(prep, "&select=[%%SEL1%%,%%SEL2%%]")
  url = paste0(object@serverURL,"/datasets/",duid,"?host=",object@folderPath)
  res = fromJSON(content(GET(url),"text"))
  new("H5S_dataset", source=src, simpleName=object@folderPath, shapes=res$shape,
      hrefs = ans, allatts=atts, presel=prep, transfermode="JSON")
}

#'getDatasetSlice from hsds server
#'
#' This function is deprecated and will be defunct in the next release.
#'
#'@param object instance of H5S_source(updated object with path to file set)
#'@param dsindex dataset index
#'@param selectionString character with selectionString
#'@param \dots unused
#'@return list of data obtained
#'@examples
#'hsdsCon = H5S_source(URL_hsds()) # hsds server
#'setPath(hsdsCon, "/home/stvjc/hdf5_mat.h5")-> hsds
#'getDatasetSlice(hsds,dsindex=1,selectionString="[1:2,1:5]")
#'@export
getDatasetSlice <- function(object, dsindex=1, selectionString, ...) {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  requireNamespace("httr")
  requireNamespace("rjson")
  uuid = getDatasetUUIDs(object)[dsindex]
  query = sprintf("%s/datasets/%s/value?host=%s&select=%s", object@serverURL, uuid, object@folderPath, selectionString)
  ans = try(GET(query, add_headers(Accept="application/json")))
  if (inherits(ans, "try-error")) stop("could not resolve select query")
  fromJSON(readBin( ans$content, what="character"))
}

#'fetch datasets of a hdf5 file from the hsds server
#'
#' This function is deprecated and will be defunct in the next release.
#'
#'@import R6
#'@import httr
#'@param object instance of H5S_source
#'@return data.frame with information about the datasets in the file 
#'@examples
#'hsdsCon = H5S_source(URL_hsds()) # hsds server
#'hsdsCon@folderPath="/home/stvjc/hdf5_mat.h5"
#'ds = fetchDatasets(hsdsCon)
#'ds
#'@export
fetchDatasets <- function(object){
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  hdf5DataStore <- R6Class("hdf5DataStore", list(
    serverURL="",
    domain="",
    rootgroup="",
    initialize=function(serverURL=NA, domain=NA){
      self$serverURL=serverURL;
      self$domain=domain;
      temp=fromJSON(content(GET(paste0(serverURL,"/domains?domain=",domain)),"text"))
      self$rootgroup=temp$domains[[1]]$root
      self$updateDatasets(temp$domains[[1]]$root, domain)
    },
    datasets=data.frame(),
    updateDatasets=function(group, domain){
      url=paste0(self$serverURL,"/groups/",group,"/links?domain=",domain)
      a=fromJSON(content(GET(url),"text"))$links
      for(i in 1:length(a)){
        a[[i]]$domain=domain
        if("collection" %in% names(a[[i]])){
          if(a[[i]]$collection=="datasets"){
            if(nrow(self$datasets)<1){
              self$datasets=as.data.frame(a[[i]],stringsAsFactors=FALSE)}else{
                self$datasets=rbind(self$datasets,
                                    as.data.frame(a[[i]],stringsAsFactors=FALSE))}
          }else{
            self$updateDatasets(a[[i]]$id,domain)
          }
        }
      }
    }
  ))
  myDS=hdf5DataStore$new(object@serverURL, object@folderPath)
  myDS$datasets
}

