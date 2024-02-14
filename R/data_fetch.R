makeCmd <- function(dataInfo, urlbase, xName, yName, zName, tName, parameter,
                    erddapXcoord, erddapYcoord, erddapTcoord, erddapZcoord,
                    verbose) {
  # build up a list with the arguments for rerddap::griddap() using do.call()
  # rerddap needs first the results from calling info
  myCallOpts <- list(dataInfo)
  if (utils::packageVersion('rerddap') < '1.0.0') {
    myCallOptsNames <- list('x')
  } else {
    myCallOptsNames <- list('datasetx')
  }
  myCallOptsNames <- list('datasetx')
  # if the url is not the default URL, add it to the list to be set
  if (!(urlbase == "https://upwell.pfeg.noaa.gov/erddap/")) {
    myCallOpts$url <- urlbase
    myCallOptsNames <- c(myCallOptsNames, 'url')
  }
  # if verbose output desired,  add to list
  if (verbose) {
    myCallOpts$callopts <- list(verbose = TRUE)
    myCallOptsNames <- c(myCallOptsNames, 'callopts')
  }
  # if xcoord exists,  add its limits
  if (!is.na(erddapXcoord[1])) {
    myCallOpts$xName <- erddapXcoord
    myCallOptsNames <- c(myCallOptsNames, xName)
  }
  # if ycoord exists,  add its limits
    if (!is.na(erddapYcoord[1])) {
    myCallOpts$yName <- erddapYcoord
    myCallOptsNames <- c(myCallOptsNames, yName)
    }
  # if zcoord exists,  add its limits
  if (!is.na(erddapZcoord[1])) {
    myCallOpts$zName <- erddapZcoord
    myCallOptsNames <- c(myCallOptsNames, zName)
  }
  # if time coordinate exists, add its limits
  if (!is.na(erddapTcoord[1])) {
    myCallOpts$tName <- erddapTcoord
    myCallOptsNames <- c(myCallOptsNames, tName)
  }
  # add the parameter name to the "fields" argument
  myCallOpts$fields <- parameter
  myCallOptsNames <- c(myCallOptsNames, 'fields')
  # tell rerddap::griddap() not to read in and melt data
  myCallOpts$read <- FALSE
  myCallOptsNames <- c(myCallOptsNames, 'read')
  # set the names of the fields added to the list
  names(myCallOpts) <- myCallOptsNames

  return(myCallOpts)
}

data_extract_read <- function(dataInfo, callDims, urlbase,
                              xName, yName, zName, tName, parameter,
                              erddapXcoord, erddapYcoord, erddapTcoord,
                              erddapZcoord,
                              verbose, cache_remove) {

  griddapCmd <- makeCmd(dataInfo, urlbase, xName, yName, zName, tName, parameter,
                        erddapXcoord, erddapYcoord, erddapTcoord, erddapZcoord,
                        verbose)
  # Get the data ------------------------------------------------------------

  numtries <- 10
  tryn <- 0
  goodtry <- -1
  while ((tryn <= numtries) & (goodtry == -1)) {
    tryn <- tryn + 1
    griddapExtract <- suppressMessages(try(do.call(rerddap::griddap, griddapCmd ), silent = TRUE))
    # if (!class(griddapExtract)[1] == "try-error") {
    if (!methods::is(griddapExtract, "try-error")) {
      goodtry <- 1
    } else{
      suppressWarnings(try(rerddap::cache_delete_all()))
      # rerddap::cache_list()
      Sys.sleep(tryn * 0.5)
    }
  }
#  if (class(griddapExtract)[1] == "try-error") {
  if (goodtry == -1) {
    print('error in trying to download the subset')
    print('check your settings')
    print(griddapCmd)
    print('stopping execution  - will return what has been downloaded so far')
    #stop('check that the dataset is active in the given ERDDAP server')
    temp_extract <- -1
    return(temp_extract)
  }


  # read in the downloaded netcdf file --------------------------------------


  datafileID <- try(ncdf4::nc_open(griddapExtract$summary$filename), silent = TRUE)
  if (methods::is(datafileID, "try-error")) {
    print('error in trying to open netcdf file')
    print('check check above for any errors')
    print('stopping execution  - will return what has been downloaded so far')
    temp_extract <- -1
    return(temp_extract)

  }

  dataX <- ncdf4::ncvar_get(datafileID, varid = xName)
  dataY <- ncdf4::ncvar_get(datafileID, varid = yName)
  if (!is.null(callDims[[3]])) {
    dataZ <- ncdf4::ncvar_get(datafileID, varid = zName)
  }

  if (!is.null(callDims[[4]])) {
    datatime <- ncdf4::ncvar_get(datafileID, varid = "time")
    datatime <- as.POSIXlt(datatime, origin = '1970-01-01', tz = "GMT")
  }

  param <- ncdf4::ncvar_get(datafileID, varid = parameter, collapse_degen = FALSE)

  ncdf4::nc_close(datafileID)


  # Readjust lat-lon coordinates --------------------------------------------

  tempCoords <- readjustCoords(param, dataX, dataY, callDims[[1]], datafileID, callDims)
  dataX <- tempCoords$dataX
  dataY <- tempCoords$dataY

  # remove netcdf file from cache
  if (cache_remove) {
    if(exists('griddapExtract')) {
    suppressWarnings(try(rerddap::cache_delete(griddapExtract), silent = TRUE))
    }
  }
  # create output list ------------------------------------------------------


  temp_extract <- list(NA, NA, NA, NA, NA, NA)
  # temp_extract <-  vector("list", 6)
  temp_extract[[1]] <- tempCoords$param
  temp_extract[[2]] <- attributes(dataInfo)$datasetid
  temp_extract[[3]] <- dataX
  temp_extract[[4]] <- dataY
  if (!is.null(callDims[[3]])) {
    temp_extract[[5]] <-  dataZ
  }
  if (!is.null(callDims[[4]])) {
    temp_extract[[6]] <-  datatime
  }
  if (grepl('etopo',temp_extract[[2]])) {
    names(temp_extract) <- c('depth', "datasetname", names(callDims)[1], names(callDims)[2], names(callDims)[3], "time")

  }else{
    names(temp_extract) <- c(parameter, "datasetname", names(callDims)[1], names(callDims)[2], names(callDims)[3], "time")

  }

  return(temp_extract)
}

