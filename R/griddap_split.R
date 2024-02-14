#' Title
#'
#' @param datasetx
#' @param ...
#' @param fields
#' @param stride
#' @param request_split
#' @param fmt
#' @param url
#' @param store
#' @param read
#' @param griddapOpts
#'
#' @return
#' @export
#'
#' @examples
griddap_split <- function(datasetx, ..., fields = 'all', stride = 1, request_split = NULL, fmt = "nc",
                    url = eurl(), store = disk(), read = TRUE, griddapOpts = list(), aggregate_file = NULL) {

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
                                    request_split, split_dim, fields, fmt, aggregate_file)
  return(extract)
}


split_griddap_request <- function(info, url, stride, griddapOpts,
                                  request_split, split_dim, fields,  fmt, aggregate_file) {
  # if fmt is duckdb,  open the connection
  if (fmt == 'duckdb') {
    aggregate_file = tempfile("extract", tmpdir = tempdir(), fileext = 'duckdb')
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

split_griddap_request1 <- function(info, url, stride, griddapOpts,
                                  request_split, split_dim, fields,  fmt, aggregate_file) {
  # if fmt is duckdb,  open the connection
  con_db <- NULL
  if (fmt == 'duckdb') {
    drv <- duckdb::duckdb()
    con_db <- duckdb::dbConnect(drv, "test.duckdb")
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


partial_extract <- function(extract, fields, fmt,  griddapOpts, stride, callopts,
                            final_result, con_db, aggregate_file) {
  names(griddapOpts)[1] <- 'datasetx'
  griddapOpts$fields <- fields
  griddapOpts$stride <- stride
  griddapOpts$callopts <- callopts
  extract <- do.call(rerddap::griddap, griddapOpts)
  if (fmt == 'memory') {
    final_result <- aggregate_memory(extract, final_result)
  } else if (fmt == 'duckdb') {
    final_result <- aggregate_duckdb(extract, con_db)
  } else if (fmt == 'nc') {
    final_result <- aggregate_netcdf(extract, aggregate_file)
  }
  return(final_result)
}




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

aggregate_memory <- function(extract, final_result){
  if (is.null(final_result)) {
    final_result <- extract
  } else {
    final_result$data <- rbind(final_result$data, extract$data)
  }
  return(final_result)
}

aggregate_duckdb <- function(extract, con_db){
  query <- "SELECT table_name FROM information_schema.tables WHERE table_name = 'extract';"
  result <- DBI::dbGetQuery(con, query)
  table_exists <- nrow(result) > 0

  if (!table_exists) {
    DBI::dbWriteTable(con_db, "extract", extract$data)
  } else {
    DBI::dbWriteTable(con_db, "extract1", extract$data, append = TRUE)
    DBI::dbExecute(con_db, "INSERT INTO extract SELECT * FROM extract1")
  }
}

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
