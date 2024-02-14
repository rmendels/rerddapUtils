#' Title Get ERDDAP gridded data restricted to a given season of the year
#'
#' \code{griddap_season} uses the R program 'rerddap::griddap()' to extract environmental data
#' from an 'ERDDAP' server in an (time, z, y ,x) bounding box where time is restricted to a
#' given season of the year  (see below).  Arguments are the same in 'rerddap::griddap()'
#' except for the added 'season' parameter.  'read' and 'fmt' options are ignored.
#' @param datasetx Anything coercable to an object of class info. So the output of a
#' call to \code{\link{info}}, or a datasetid, which will internally be passed
#' through \code{\link{info}
#' @param ... Dimension arguments. See examples. Can be any 1 or more of the
#' dimensions for the particular dataset - and the dimensions vary by dataset.
#' For each dimension, pass in a vector of length two, with min and max value
#' desired. at least 1 required.
#' @param fields (character) Fields to return, in a character vector.
#' @param stride (integer) How many values to get. 1 = get every value, 2 = get
#' @param season (character) a character array with the times of the trajectory in
#'  the form  c("MM-DD", "MM-DD")
#' @param fmt (character) One of csv or nc (for netcdf). Default: nc
#' @param url A URL for an ERDDAP server. Default:
#' https://upwell.pfeg.noaa.gov/erddap/ - See [eurl()] for
#' more information
#' @param store One of \code{\link{disk}} (default) or \code{\link{memory}}. You
#' can pass options to \code{\link{disk}}. Beware: if you choose \code{fmt="nc"},
#' we force \code{store=disk()} because nc files have to be written to disk.
#' @param read (logical) Read data into memory or not. Does not apply when
#' \code{store} parameter is set to memory (which reads data into memory).
#' For large csv, or especially netcdf files, you may want to set this to
#' \code{FALSE}, which simply returns a summary of the dataset - and you can
#' read in data piecemeal later. Default: \code{TRUE}
#' @param callopts Curl options passed on to \code{\link[crul]{verb-GET}}
#'
#' @return An object of class \code{griddap_csv} if csv chosen or
#' \code{griddap_nc} if nc file format chosen.
#' @export
#'
#' @examples
griddap_season <- function(datasetx, ..., fields = 'all', stride = 1, season = NULL, fmt = "nc",
                          url = eurl(), store = disk(), read = TRUE, callopts = list()) {

  x <- datasetx
  if (is.null(season)) {
    print('no season is given')
    print('this must be a vector lenght 2')
    print('each element of is of form month-day')
    stop('stopped on error')
  }
  dimargs <- list(...)
  if (length(dimargs) == 0) stop("no dimension arguments passed, see ?griddap")
  if (inherits(x, "info")) {
    url <- x$base_url
    message("info() output passed to x; setting base url to: ", url)
  } else {
    x <- rerddap::as.info(x, url)
  }
  if (attr(x, "type") != "griddap")
    stop("datasetid '", attr(x, "datasetid"), "' not of type griddap")
  check_dims(dimargs, x)
  if (!is.null(dimargs$time)) {
    check_time_range(dimargs, x)
  }
  call_list <- extract_rerddap_call(x, dimargs, stride, fields, url)
  dimargs <- call_list$dimargs
  dim_args <- call_list$dim_args
  dimVals <- call_list$dimVals
  url_base <- call_list$url_base
  fields <- call_list$fields
  extract <- griddap_season_request(x, url_base, dimargs, dim_args, fields, season)
  return(extract)
}

# 'griddap_season_request()' is a function to take the base 'rerddap::griddap()' type  request and
# loop over years to make an extract only for the given season and combine
# results into one griddap list.
# parameters:
#   info - result of call to rerddap::info
#   url_base - base url for griddap call
#   dimargs -
#   dim_args
#   fields - name or variables to include in extract
#   seasons - season of year to restrict request.
# return:
#   normal griddap structure combined over the seasons,
griddap_season_request <- function(info, url_base, dimargs, dim_args, fields, season) {
  start_year_dt <- lubridate::as_datetime(dimargs$time[1])
  end_year_dt <- lubridate::as_datetime(dimargs$time[2])
  start_year <- lubridate::year(start_year_dt)
  end_year <- lubridate::year(end_year_dt)
  years <- seq(start_year_dt, end_year_dt, 'years')
  CallOptsNames <- c('datasetx',  names(dimargs), 'fields')
  no_dims <- length(names(dimargs))
  extract = NULL
  for (year_index in seq_along(years)) {
    my_year <- as.character(lubridate::year(years[year_index]))
    CallOpts <- list(info)
    temp_time1 <- paste0(my_year, '-', season[1])
    temp_time2 <- paste0(my_year, '-', season[2])
    my_time <- c(temp_time1, temp_time2)
    CallOpts$time <- my_time
    for (my_dim in names(dimargs)[2:no_dims]) {
      CallOpts[[my_dim]] <- dimargs[[my_dim]]
    }
    CallOpts$fields <- fields
    season_extract <- do.call(rerddap::griddap, CallOpts)
    if (is.null(extract)) {
      extract <- season_extract
    } else {
      extract$data <- rbind(extract$data, season_extract$data)
    }
  }
  return(extract)
}
