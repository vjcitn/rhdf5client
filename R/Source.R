deprecate_msg = paste0("This function is deprecated. The new interface to rhdf5client",
 " is exclusively through its DelayedArray backend HSDSArray")

#' An S4 class to represent a HDF5 server listening on a port.
#'
#' This class is deprecated and will be defunct in the next release.
#'
#' @slot endpoint URL for server 
#' @slot type Type of server software at the source; must be 
#' either 'h5serv' or (default) 'hsds' 
setClass("HSDSSource", representation(endpoint="character", type="character"))

#' Construct an object of type HSDSSource.
#'
#' A HSDSSource is a representation of a URL which provides access to a HDF5 
#' server (either h5serv or hsds.) 
#'
#' This function is deprecated and will be defunct in the next release.
#'
#' @name HSDSSource
#' @param endpoint URL for server 
#' @param type Type of server software at the source; must be 
#' @return An object of type HSDSSource
#' @examples
#' src.hsds <- HSDSSource('http://hsdshdflab.hdfgroup.org')
#' @export
HSDSSource <- function(endpoint, type='hsds')  {
  #.Deprecated("HSDSArray", NULL, deprecate_msg)
  if (!(type %in% c('h5serv', 'hsds')))
    stop(paste("unknown server type ", type))
  obj <- new("HSDSSource", endpoint=endpoint, type=type)
  # member root id also?
}


.HSDSSource <- function(endpoint, type='hsds')  { # after deprecation cycle this private function is used
  if (!(type %in% c('h5serv', 'hsds')))
    stop(paste("unknown server type ", type))
  # obj <- new("HSDSSource", endpoint=endpoint, type=type) # should not use assignment as function value
  new("HSDSSource", endpoint=endpoint, type=type)
}

#' List files and subdirectories of a domain
#'
#' The user needs to give the domain to start in. The search
#' will be non-recursive. I.e., output for domain '/home/jreadey/' will 
#' not return the files in '/home/jreadey/HDFLabTutorial/'
#'
#' This function is deprecated and will be defunct in the next release.
#'
#' @param object An object of type HSDSSource 
#'
#' @param rootdir A slash-separated directory in the HSDSSource file system. 
#'
#' @return a vector of domains in the rootdir
#'
#' @export
#' @docType methods
#' @rdname listDomains-methods
#'
#' @examples
#' src.hsds <- HSDSSource('http://hsdshdflab.hdfgroup.org')
#' src.chan <- HSDSSource('http://h5s.channingremotedata.org:5000', 'h5serv')
#' listDomains(src.chan)
#' listDomains(src.hsds, '/home/jreadey')
setGeneric('listDomains', function(object, rootdir) standardGeneric('listDomains'))

#' @rdname listDomains-methods
#' @aliases listDomains,HSDSSource,character-method
setMethod('listDomains', c("HSDSSource", "character"), 
  function(object, rootdir)  {
    #.Deprecated("HSDSArray", NULL, deprecate_msg)
    ll <- domainContents(object, rootdir)
    vapply(ll, function(l) l$filename, character(1))
  })

#' @rdname listDomains-methods
#' @aliases listDomains,HSDSSource,missing-method
setMethod('listDomains', c("HSDSSource", "missing"),  
  function(object) { 
    #.Deprecated("HSDSArray", NULL, deprecate_msg)
    listDomains(object, '/hdfgroup/org') 
  })

# private
domainContents <- function(object, rootdir = '/hdfgroup/org')  {
  if (!(is(object, "HSDSSource")))
    stop("getDomains called on a non-HSDSSource object")

  # result list
  rlistsz <- 1000
  rlist <- vector("list", rlistsz)
  nrlist <- 1

  append_to_results <- function(fn, ft)   {
    rlist[[nrlist]] <<- list(filename=fn, filetype=ft)
    nrlist <<- nrlist + 1
    if (nrlist >= rlistsz)  
      stop("list overflow")
  }

#  The h5serv API does not have a GET /domains method, so we need
#  to descend the tree to the requested domain node by UUID's.

  result <- tryCatch({
    if (object@type == 'h5serv')  {
      append_if_h5_file <- function(ll, ff)  {
        if ('h5domain' %in% names(ll)) {
          ff <- c(ff, ll[['h5domain']])
        } 
        ff
      }

      # dissect the path. assume the last two elements are the root domain. 
      s <- .Platform$file.sep
      if (substr(rootdir, 1, 1) != s) rootdir <- paste0(s, rootdir)
      pth <- strsplit(rootdir, s)[[1]]
      pth <- pth[-1]      # empty string before leading '/'
      n <- length(pth)
      dstr <- paste0('.', pth[n-1], '.', pth[n])   # root domain
      if (length(pth) == 2)
        pth = c()
      else
        pth <- pth[1:(n-2)]

      request <- paste0(object@endpoint, "/")
      response <- submitRequest(request)
      nextid <- response[['root']]
      link <- list()

      while (length(pth) > 0)  {

        request <- paste0(object@endpoint, "/groups/", nextid, "/links")
        response <- submitRequest(request)
        links <- response[['links']]

        v <- vapply(links, function(lk) { 
          'title' %in% names(lk) && lk[['title']] == pth[length(pth)] }, 
          logical(1))
        if (any(v))  {
          link <- links[[which(v)]]
          nextid <- link[['id']]
          dstr <- paste0(pth[length(pth)], '.', dstr)
          pth <- pth[-length(pth)]
        }
        else  {    # special case: rootdir is a file 
          v <- vapply(links, function(lk) { 
            'class' %in% names(lk) && lk[['class']] == 'H5L_TYPE_EXTERNAL' && 
            'h5domain' %in% names(lk) && lk[['h5domain']] == pth[length(pth)] }, 
            logical(1))
          if (any(v))  {
            link <- links[[which(v)]]
            pth <- c()
          } else  {
            stop(paste0("domain ",rootdir, " not found"))
          }
        }
      }
  
      if (length(link) == 0 || 
          link[['class']] == 'H5L_TYPE_HARD')  {  # domain is a directory
        request <- paste0(object@endpoint, "/groups/", nextid, "/links")
        response <- submitRequest(request)
        links <- response[['links']]
        for (lk in links)  {
          if (lk[['class']] == 'H5L_TYPE_HARD')  {             # a subdirectory
            append_to_results(paste0(lk[['title']], rootdir), 'directory')
          } else if (lk[['class']] == 'H5L_TYPE_EXTERNAL')  {  # a file
            append_to_results(lk[['h5domain']], 'file')
          }
        }

      } else  {  # 'self' signals that rootdir *is* a file 
        append_to_results(rootdir, 'self')
      }
      1
    }
    else  { # 'hsds'

      # no postpended slash gets the type of the object
      request <- paste0(object@endpoint, "/domains?domain=", rootdir)
      response <- submitRequest(request)
      if ('domains' %in% names(response) && length(response[['domains']]) > 0)  {

        # with postpended slash gets directory contents
        if ('root' %in% names(response[['domains']][[1]]) && 
            response[['domains']][[1]][['root']] != 'None')  {
          fn <- rootdir
          ft <- 'self'
          append_to_results(fn, ft)
        } else  {
          request <- paste0(object@endpoint, "/domains?domain=", rootdir, "/")
          response <- submitRequest(request)
          domains <- response[['domains']]
          for (domain in domains)  {
            if ('class' %in% names(domain) && 'name' %in% names(domain) && 
               (domain[['class']] == 'domain' || domain[['class']] == 'folder'))  {
              fn <- domain[['name']]
              ft <- 'directory'
              if (domain[['class']] == 'domain') 
                ft <- 'file'
              append_to_results(fn, ft)
            }
          }
        }
      }
      1
    }
  }, error=function(e) { -1 })    # catch http request errors
  if (result == -1)  {
    warning("bad http request", call. = FALSE)
  }

  rlist[-which(sapply(rlist, is.null))]

}

# private - submit request and handle errors 
submitRequest <- function(req, transfermode='JSON')  {
  rsp <- tryCatch({
      if (transfermode == 'JSON')  {
        httr::GET(req)
      } else if (transfermode == 'binary')  {
        httr::GET(req, add_headers(Accept="application/octet-stream"))
      }
    }, error=function(e) { NULL }
  )

  if (is.null(rsp))  
    stop("Bad request")

  if ('status_code' %in% names(rsp) && rsp$status_code != 200)  
    stop("Bad request")

  if (transfermode == 'JSON')  {
    rsp <- tryCatch({
      rjson::fromJSON(readBin(rsp$content, what="character"))
    }, error=function(e) { NULL })
  }

  if (is.null(rsp))  
    stop("Bad request")

  # Note: rsp could be the empty string - not an error
  # maybe throw a warning condition?

  return(rsp)

}


