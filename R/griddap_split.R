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
#' @param request_split A numeric vector indicating the number of splits for each dimension, used
#'   to segment the request into manageable chunks. This is particularly useful for large datasets.
#' @param fmt (character) One of:
#' -  nc: save output to a netcdf file
#' -  memory:  save output in a dataframe in memory
#' -  duckdb:  save data in a duckdb database
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
#' @param aggregate_file A string specifying:
#'  - if is.null(aggregate_file) then a temporary file is created that does not
#'       overwrite user space
#'  - if "fmt = nc" the path to the NetCDF file to store the results
#'  - if "fmt = duckdb" the path to the duckdb file to store the results
#'  - if "fmt = memory" the value is ignored
#' @return Varies by format requested:
#'  - if "fmt = nc" the path to the NetCDF file where the results are stored
#'  - if "fmt = duckdb" the path to the duckdb file where the results are stored
#'  - if "fmt = memory" the usual rerddap::griddap dataframe
#' @export
#'
#' @examples
griddap_split <- function(datasetx, ..., fields = 'all', stride = 1, request_split = NULL, fmt = "nc",
                    url = eurl(), store = disk(), read = TRUE, callopts = list(), aggregate_file = NULL) {

  if(is.null(aggregate_file) & (fmt =='nc')) {
    aggregate_file = tempfile("extract", tmpdir = tempdir(), fileext = 'nc')
  }
  x <- datasetx
  if (is.null(request_split)) {
    print('no split is given')
    print('this must be a vector the same length of the number of dimensions')
    print('each elementof the vector is the number of splits in that dimension')
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
  dimargs = call_list$dimargs
  dim_args = call_list$dim_args
  dimVals = call_list$dimVals
  url_base = call_list$url_base
  fields <- call_list$fields
  nc_file <- call_list$nc_file
  split_dim <- define_split(dimVals, request_split)
  # if fmt is netcdf,  create new netcdf file and copy attributes
  if (fmt == 'nc') {
    return <- create_nc_file(x, fields, nc_file, aggregate_file )
    return <- copy_attributes(x, fields, nc_file, aggregate_file)
  }
  extract <- split_griddap_request1(x, url, stride, griddapOpts,
                                    request_split, split_dim, fields, fmt,
                                    callopts, aggregate_file)
  return(extract)
}


split_griddap_request <- function(info, url, stride, griddapOpts,
                                  request_split, split_dim, fields,  fmt,
                                  callopts, aggregate_file) {
  # if fmt is duckdb,  open the connection
  if (fmt == 'duckdb') {
    if (is.null(aggregate_file)) {
      aggregate_file = tempfile("extract", tmpdir = tempdir(), fileext = 'duckdb')
     }
    drv <- duckdb::duckdb()
    con_db <- duckdb::dbConnect(drv, aggregate_file)
  }
  griddapOptsNames <- c('datasetx',  names(split_dim), 'fields')
  split_names <- names(split_dim)
  final_result <- NULL
  for (i in seq(1, request_split[[1]])) {
    temp_name <- split_names[1]
    temp_value <- get_dim_constraint(request_split, split_dim, 1,  i)
    griddapOpts <- list(info)
    griddapOpts[[temp_name]] <- temp_value
    for (j in seq(1, request_split[[2]])) {
      temp_name <- split_names[2]
      temp_value <- get_dim_constraint(request_split, split_dim, 2,  j)
      griddapOpts[[temp_name]] <- temp_value
      for (k in seq(1, request_split[[3]])) {
        temp_name <- split_names[3]
        temp_value <- get_dim_constraint(request_split, split_dim, 3,  k)
        griddapOpts[[temp_name]] <- temp_value
        for (l in seq(1, request_split[[4]])) {
          temp_name <- split_names[4]
          temp_value <- get_dim_constraint(request_split, split_dim, 4,  l)
          griddapOpts[[temp_name]] <- temp_value
          final_result <- partial_extract(extract, fields, fmt,  griddapOpts,
                                          final_result, con_db, aggregate_file)
       }
      }
    }
  }
  if (fmt == 'duckdb') {
    duckdb::dbDisconnect(con_db, shutdown = TRUE)
    final_result <- aggregate_file
  } else if (fmt == 'nc') {
    final_result <- aggregate_file
  }
  return(final_result)
}

# Split and Process 'rerddap::griddap()' Requests
#
# This function handles splitting and processing requests to the `rerddap::griddap()` service,
# facilitating the handling of large datasets by dividing the requests into manageable parts.
# It supports output in different formats, including NetCDF ('nc') and DuckDB ('duckdb'), and
# performs the necessary operations to aggregate the results into a single file or database.
#
# @param info An object containing the dataset information, typically obtained from a call to
#  `rerddap::info()` or similar function that provides metadata about the dataset.
# @param url The base URL of the ERDDAP server from which to fetch the data.
# @param stride An integer indicating the stride (step size) for the query, allowing for
#   data thinning to reduce the volume of data retrieved.
# @param griddapOpts A list of options to pass to the `rerddap::griddap()` call, potentially
#   including additional parameters like authentication credentials.
# @param request_split A numeric vector indicating how to split the dataset query along each
#   dimension to manage large datasets more effectively.
# @param split_dim A named list where each name corresponds to a dimension in the dataset and
#   the values indicate how to split each dimension for the request.
# @param fields A character vector specifying which fields (variables) to retrieve from the
#   dataset. A value of 'all' indicates that all available fields should be included.
# @param fmt The format of the output, with supported values including 'nc' for NetCDF files
#   and 'duckdb' for DuckDB database format.
# @param aggregate_file The path to the output file or database where the aggregated results
#   from the split requests should be stored.
#
# @return Depending on the `fmt`, either the path to the aggregated NetCDF file or the DuckDB
#   database file containing the data retrieved and processed from the split requests.
#
# @examples
# # Assuming `info` is a valid dataset information object and the necessary parameters are defined:
# result <- split_griddap_request1(info, "https://example.com/erddap/", 1, list(),
#                                 c(2, 2), list(lat = 2, lon = 2), 'all', 'nc', "output.nc")
# print(result)
#
split_griddap_request1 <- function(info, url, stride, griddapOpts,
                                  request_split, split_dim, fields,  fmt,
                                  callopts, aggregate_file) {
  # if fmt is duckdb,  open the connection
  con_db <- NULL
  if (fmt == 'duckdb') {
    if (is.null(aggregate_file)) {
      aggregate_file = tempfile("extract", tmpdir = tempdir(), fileext = 'duckdb')
    }
    drv <- duckdb::duckdb()
    con_db <- duckdb::dbConnect(drv, aggregate_file)
  }
  griddapOptsNames <- c('datasetx',  names(split_dim), 'fields', "stride", "callopts")
  split_names <- names(split_dim)
  final_result <- NULL
  griddapOpts <- list(info)
  final_result <- recursive_extract(1, request_split, split_dim, griddapOpts,
                                    stride, callopts, final_result,
                                    info, fields, fmt, con_db, aggregate_file)
  if (fmt == 'duckdb') {
    duckdb::dbDisconnect(con_db, shutdown = TRUE)
    final_result <- aggregate_file
  } else if (fmt == 'nc') {
    final_result <- aggregate_file
  }
  return(final_result)
}


# Recursively Split and Extract Data Requests
#
# This function is designed to recursively split and process data requests based on specified
# dimensions and constraints. It is particularly useful for managing large datasets or requests
# that need to be broken down into smaller chunks to comply with server limitations or to
# optimize data retrieval. The function iterates through each dimension, applies constraints,
# and accumulates the results.
#
# @param level The current level of recursion, indicating the dimension being processed.
# @param request_split A vector specifying the number of splits for each dimension of the data
#   request. The length of this vector should match the number of dimensions in the dataset.
# @param split_dim A named list where each name corresponds to a dimension and each value
#   specifies the splitting criteria for that dimension.
# @param griddapOpts A list of options for the `rerddap::griddap()` call, modified recursively
#   to reflect the current split criteria.
# @param stride The stride (step size) for the data retrieval, which can help reduce the volume
#   of data retrieved by skipping over points.
# @param callopts A list of additional options to pass to the HTTP request function, useful for
#   configuring details such as timeouts or proxy settings.
# @param final_result An accumulator for the results from each recursive call, which can be
#   aggregated or processed further.
# @param info Dataset information, typically obtained from a call to `rerddap::info()` or a
#   similar function, providing metadata about the dataset.
# @param fields Specifies which fields (variables) to retrieve from the dataset. Can be 'all'
#   for retrieving all fields or a vector of specific field names.
# @param fmt The format of the output, which can influence how results are stored or aggregated.
# @param con_db A connection object for database formats (e.g., DuckDB) when `fmt` is set to
#   a database format. This parameter is used to manage database connections during data extraction.
# @param aggregate_file The path to the file where aggregated results should be stored, applicable
#   when data is being compiled into a single file.
#
# @return The final aggregated result of the data extraction process, the specifics of which
#   depend on the `fmt` parameter and the nature of the `final_result` accumulation.
#
# @examples
# # This is a hypothetical example since the actual call depends on specific dataset information,
# # dimensions, and server configurations:
# result <- recursive_extract(1, c(2, 2), list(lat = 2, lon = 2), list(),
#                             1, list(), NULL, dataset_info, 'all', 'nc', NULL, "output.nc")
# print(result)
#
recursive_extract <- function(level, request_split, split_dim, griddapOpts,
                              stride, callopts, final_result,
                              info, fields, fmt, con_db, aggregate_file) {
  split_names <- names(split_dim)

  # Base case: If the level exceeds the length of request_split, perform the extraction
  if (level > length(request_split)) {
    final_result <- partial_extract(extract, fields, fmt, griddapOpts,  stride, callopts,
                                    final_result, con_db, aggregate_file)
    return(final_result)
  }

  # Recursive step: Iterate over the current dimension
  temp_name <- split_names[level]
  for (i in seq(1, request_split[[level]])) {
    temp_value <- get_dim_constraint(request_split, split_dim, level, i)
    griddapOpts[[temp_name]] <- temp_value
    # Recurse to the next level with updated griddapOpts
    final_result <- recursive_extract(level + 1, request_split, split_dim, griddapOpts,
                                      stride, callopts, final_result,
                                      info, fields, fmt, con_db, aggregate_file)
  }

  return(final_result)
}

# Perform a Partial Data Extraction and Aggregation
#
# This function facilitates a portion of the data extraction process from a `rerddap::griddap()` call,
# handling specific fields and formatting options. It supports aggregating the extracted data into
# various formats such as in-memory objects, DuckDB, or NetCDF files, based on the specified format.
#
# @param extract A placeholder parameter for consistency in function signature; not used.
# @param fields The fields (variables) to be extracted from the dataset.
# @param fmt The format for aggregating the extracted data: 'memory', 'duckdb', or 'nc' (NetCDF).
# @param griddapOpts A list of options for the `rerddap::griddap()` call, including dataset ID,
#   dimensions, and any other parameters necessary for the data request.
# @param stride An integer specifying the stride (step size) for the data retrieval, useful for
#   thinning the data.
# @param callopts A list of additional options to customize the HTTP request made by `rerddap::griddap()`.
# @param final_result An accumulator for the results, which varies based on the specified format. For
#   'memory', this would be a data structure holding the aggregated data; for 'duckdb' or 'nc', it might
#   not be directly used.
# @param con_db A connection object for the DuckDB database, used when `fmt` is 'duckdb'.
# @param aggregate_file The file path for storing the aggregated results when `fmt` is 'nc'.
#
# @return Depending on the specified `fmt`, returns the aggregated data in the chosen format. For
#   'memory', it returns the aggregated data structure; for 'duckdb', it may return the connection
#   object or an indication of success; for 'nc', it may return the path to the NetCDF file or an
#   indication of success.
#
# @examples
# # Example usage (hypothetical, depends on setup and dataset):
# griddapOpts <- list(datasetid = 'exampleDataset', time = c('2020-01-01', '2020-01-31'),
#                     latitude = c(30, 45), longitude = c(-125, -115), altitude = c(0, 100))
# result <- partial_extract(NULL, 'temperature', 'memory', griddapOpts, 1, list(),
#                           NULL, NULL, "data.nc")
# print(result)
#
partial_extract <- function(extract, fields, fmt,  griddapOpts, stride, callopts,
                            final_result, con_db, aggregate_file) {
  names(griddapOpts)[1] <- 'datasetx'
  griddapOpts$fields <- fields
  griddapOpts$stride <- stride
  griddapOpts$callopts <- callopts
  extract <- suppressMessages(do.call(rerddap::griddap, griddapOpts))
  if (fmt == 'memory') {
    final_result <- aggregate_memory(extract, final_result)
  } else if (fmt == 'duckdb') {
    final_result <- aggregate_duckdb(extract, con_db)
  } else if (fmt == 'nc') {
    final_result <- aggregate_netcdf(extract, aggregate_file)
  }
  return(final_result)
}



# Calculate Dimension Constraints for Data Retrieval Requests
#
# This function determines the constraints for a specific dimension of a dataset, based on the
# provided splitting criteria. It's designed to support segmented data retrieval operations by
# calculating the subset bounds for each segment of the request. This is particularly useful in
# the context of large dataset queries where requests may need to be split to avoid server
# limitations or to manage memory more efficiently.
#
# @param request_split A numeric vector indicating the number of splits for each dimension of
#   the data request. The length of this vector should match the number of dimensions in the dataset.
# @param split_dim A list where each element corresponds to a dimension in the dataset and contains
#   the full range of values for that dimension. The list's names should match the dimension names.
# @param var_index The index (integer) of the variable within `split_dim` for which the constraint
#   is being calculated.
# @param loop_index The index (integer) within the specified split of the dimension to calculate
#   the constraint for. This helps identify the specific segment of the split dimension.
#
# @return A numeric vector of length two, representing the lower and upper bounds of the constraint
#   for the specified dimension segment. If the dimension is not split (`request_split[var_index] == 1`),
#   it returns the full range for that dimension.
#
# @examples
# # Example usage for a dataset with dimensions split into segments:
# request_split <- c(2, 3) # Assuming two dimensions, split into 2 and 3 segments, respectively
# split_dim <- list(time = list(c('2020-01-01', '2020-06-30'), c('2020-07-01', '2020-12-31')),
#                   depth = list(c(0, 50), c(51, 100), c(101, 150)))
# var_index <- 1 # For 'time'
# loop_index <- 2 # Second segment of 'time'
# constraint <- get_dim_constraint(request_split, split_dim, var_index, loop_index)
# print(constraint)
#
get_dim_constraint <- function(request_split, split_dim, var_index,  loop_index){
  split_names <- names(split_dim)
  temp_name <- split_names[var_index]
  if(request_split[var_index] == 1) {
    temp_value <- split_dim[[var_index]]
    temp_value <- unlist(temp_value)
    temp_value <- c(temp_value[1], temp_value[length(temp_value)])
  } else {
    temp_value <- split_dim[[var_index]]
    temp_value <- temp_value[loop_index]
    temp_value <- unlist(temp_value, use.names=FALSE)
    temp_value <- c(temp_value[1], temp_value[length(temp_value)])
  }
  return(temp_value)
}

# Define Splits for Dimension Values Based on Requested Segments
#
# This function calculates and organizes the splits for each dimension of a dataset based on the
# specified number of segments. It is useful for preparing segmented data retrieval requests,
# particularly when working with large datasets or when server limitations necessitate breaking down
# requests into smaller chunks.
#
# @param dimVals A list where each element corresponds to a dimension in the dataset and contains
#   the full range of values for that dimension. The list's names should match the dimension names.
# @param request_split A numeric vector indicating the number of segments into which each dimension
#   should be split. The length of this vector should match the number of dimensions in `dimVals`.
#
# @return A list of the same structure as `dimVals`, where each dimension's values are further
#   organized into the specified number of segments. If a dimension is not to be split (`request_split`
#   value of 1), its original range is preserved.
#
# @examples
# dimVals <- list(
#   time = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="month"),
#   depth = seq(0, 500, by=100)
# )
# request_split <- c(4, 2) # Split time into 4 segments and depth into 2 segments
#
# # Define the splits
# splits <- define_split(dimVals, request_split)
#
# print(splits)
#
define_split <- function(dimVals, request_split) {
  indices <- request_split > 1
  dim_splits <- list()
  for (i in seq_along(dimVals)) {
    temp_name <- names(dimVals)[i]
    temp_dim <- dimVals[[i]]
    no_cut <- request_split[[i]]
    if (no_cut == 1) {
      dim_splits[[temp_name]] <- temp_dim
    } else {
      dim_splits[[temp_name]] <- split(temp_dim, cut(seq_along(temp_dim), no_cut, labels = FALSE))
    }
  }
  return(dim_splits)
}

# Aggregate Extracted Data in Memory
#
# This function aggregates newly extracted data with an existing data structure in memory.
# It is designed to sequentially combine data extracts, typically used when data is being
# retrieved in segments and needs to be compiled into a single dataset.
#
# @param extract A data frame or list representing the newly extracted data to be aggregated.
#   This parameter is expected to have a structure compatible with `final_result`, particularly
#   in terms of column names and types if it's a data frame.
# @param final_result The current aggregate of previously extracted data. If `NULL`,
#   `extract` becomes the initial dataset for aggregation. This parameter should be a data
#   structure (e.g., a data frame or list) that can hold the combined result of successive
#   extractions.
#
# @return Returns an updated version of `final_result` that includes the data from `extract`.
#   If `final_result` was initially `NULL`, returns `extract`.
#
# @examples
# # Assuming `extract1` and `extract2` are data frames with similar structures:
# final_result <- NULL
# final_result <- aggregate_memory(extract1, final_result)
# final_result <- aggregate_memory(extract2, final_result)
# print(final_result)
#
aggregate_memory <- function(extract, final_result){
  if (is.null(final_result)) {
    final_result <- extract
  } else {
    final_result$data <- rbind(final_result$data, extract$data)
  }
  return(final_result)
}

#Aggregate Extracted Data into a DuckDB Database
#
#This function facilitates the aggregation of newly extracted data into a DuckDB database.
#It checks if a table named 'extract' exists in the database and, depending on its presence,
#either creates the table and inserts the data or appends the data to an existing table.
#
#@param extract A data frame or list representing the newly extracted data to be aggregated
#  into the DuckDB database. This parameter should contain the actual data in a format
#  compatible with DuckDB table structures.
#@param con_db A DuckDB connection object created via `DBI::dbConnect()`. This connection
#  is used to interact with the DuckDB database for writing the extracted data.
#
#@return This function does not return a value but performs an operation that inserts or
#  appends data into a DuckDB database.
#
#@examples
## Assuming `extract` is a data frame with the extracted data, and `con_db` is an
## established DuckDB connection:
## Connect to DuckDB database (example connection, replace with actual connection details)
#con_db <- DBI::dbConnect(duckdb::duckdb(), dbname = "path/to/your/database.duckdb")
#
#aggregate_duckdb(extract, con_db)
#
## After aggregation, don't forget to disconnect
#DBI::dbDisconnect(con_db)
#
aggregate_duckdb <- function(extract, con_db){
  # Corrected function body with appropriate variable names
  query <- "SELECT table_name FROM information_schema.tables WHERE table_name = 'extract';"
  result <- DBI::dbGetQuery(con_db, query) # Corrected the connection object variable name
  table_exists <- nrow(result) > 0

  if (!table_exists) {
    DBI::dbWriteTable(con_db, "extract", extract$data)
  } else {
    DBI::dbWriteTable(con_db, "extract1", extract$data, append = TRUE)
    DBI::dbExecute(con_db, "INSERT INTO extract SELECT * FROM extract1")
    # Assuming cleanup is needed for the temporary table 'extract1'
    DBI::dbExecute(con_db, "DROP TABLE extract1")
  }
}


aggregate_duckdb_old <- function(extract, con_db){
  query <- "SELECT table_name FROM information_schema.tables WHERE table_name = 'extract';"
  result <- DBI::dbGetQuery(con_db, query)
  table_exists <- nrow(result) > 0

  if (!table_exists) {
    DBI::dbWriteTable(con_db, "extract", extract$data)
  } else {
    DBI::dbWriteTable(con_db, "extract1", extract$data, append = TRUE)
    DBI::dbExecute(con_db, "INSERT INTO extract SELECT * FROM extract1")
  }
}

# Aggregate Extracted Data into a NetCDF File
#
# This function aggregates extracted data into an existing NetCDF file. It is particularly useful
# for sequentially adding data from multiple extracts into a single NetCDF file, handling
# alignment of dimensions and variables between the extract and the aggregate file. It supports
# time dimensions and other variables, converting time values appropriately if necessary.
#
# @param extract A list representing the newly extracted data, including metadata about the
#   extraction (e.g., file names, dimensions, variables). The `summary` element of this list
#   should contain the filename, dimensions (`dim`), and variables (`var`) of the extracted data.
# @param aggregate_file A string specifying the path to the NetCDF file into which the extracted
#   data should be aggregated. This file should already exist and be structured to accommodate
#   the extracted data.
#
# @return This function does not return a value but modifies the specified NetCDF file by
#   adding or updating data from the extract.
#
# @examples
# # Assuming `extract` contains the necessary structure and `aggregate_file` is the path to
# # your NetCDF file:
# aggregate_netcdf(extract, "path/to/your/aggregate_file.nc")
#
# # Note: This example assumes that `extract` and `aggregate_file.nc` are correctly prepared
# # and compatible. Actual usage will require specific setup of the NetCDF file and extracts.
#
aggregate_netcdf <- function(extract, aggregate_file){
    extract_file_name <- extract$summary$filename
    extract_file_root <- ncdf4::nc_open(extract_file_name)
    root <-  ncdf4::nc_open(aggregate_file, write = TRUE)
    dim_names <- names(extract$summary$dim)
    field_names <- names(extract$summary$var)
    start <- list()
    count <- list()
    for (dim_name in dim_names) {
      extract_dim_values <- extract$data[[dim_name]]
      if(dim_name == 'time') {
        extract_dim_values <- lubridate::as_datetime(extract_dim_values)
        extract_dim_values <- as.numeric(extract_dim_values)
      }
      file_dim_values <- ncdf4::ncvar_get(root, dim_name)
      dim_indices <- which(file_dim_values >= min(extract_dim_values) & file_dim_values <= max(extract_dim_values))
      start[[dim_name]] <- dim_indices[1]
      count[[dim_name]] <- length(dim_indices)
    }
    start <-rev( unlist(start))
    count = rev(unlist(count))
    for (field in field_names) {
      vals <- ncdf4::ncvar_get(extract_file_root, field)
      ncdf4::ncvar_put(root, field, vals, start = start, count = count)
    }
    ncdf4::nc_close(root)
    ncdf4::nc_close(extract_file_root)
}
