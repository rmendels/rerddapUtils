get_dimVals <- function(x, dimargs) {
  base_url <- paste0(x$base_url, '/')
  call_args <- list(datasetx = x)
  for (i in seq_along(dimargs)){
    call_args <- c(call_args, dimargs[i])
    names(call_args)[i + 1] <- names(dimargs)[i]
  }
  call_args$fields <- 'none'
  call_args$read = FALSE
  call_args$url <- base_url
  temp_result <- do.call(rerddap::griddap, call_args)
  dimVals = list()
  for (i in seq_along(temp_result$summary$dim)){
    temp <- temp_result$summary$dim[[i]]
    dimVals[[temp$name]] <- temp$vals
  }
  if ('time' %in% names(dimVals)) {
    dimVals$time <- as.character(as.POSIXlt(dimVals$time, origin = '1970-01-01', tz = "GMT"))
  }
  return(list(dimVals = dimVals, nc_file = temp_result$summary$filename))
}

extract_rerddap_call <- function(x, dimargs, stride, fields, url) {
  check_lat_text(dimargs)
  check_lon_text(dimargs)
  dimargs <- fix_dims(dimargs, .info = x)
  check_lon_data_range(dimargs, x)
  check_lat_data_range(dimargs, x)
  d <- attr(x, "datasetid")
  var <- field_handler(fields, x$variables$variable_name)
  dims <- dimvars(x)
  #store <- toggle_store(fmt, store)
  pargs <- sapply(dims, function(y) parse_args(x, y, stride, dimargs))
  dim_args <- list()
  for (i in seq_along(dims)){
    dim_args[[dims[i]]] <- paste0(dims[i], pargs[i])
  }
  #dim_args <- paste(temp_args, collapse = ',')
  #args <- lapply(dims, function(y) {
  #  paste0(y, paste0(pargs, collapse = ""))
  # })
  #fmt <- match.arg(fmt, c("nc", "csv"))
  fmt <- "nc"
  lenURL <- nchar(url)
  if (substr(url, lenURL, lenURL) != '/') {
    url <- paste0(url, '/')
  }
  url_base <- sprintf("%sgriddap/%s.%s", url, d, fmt)
  dimVals_extract <- get_dimVals(x, dimargs)
  dimVals <- dimVals_extract$dimVals
  nc_file <- dimVals_extract$nc_file
  call_info <- list(dimargs = dimargs, dim_args = dim_args,
                    dimVals = dimVals, url_base = url_base,
                    fields = var, nc_file = nc_file)
  return(call_info)
}



define_split_nc <- function(source_nc, split){
  no_time <- source_nc$dim[['time']]$len
  no_altitude <- source_nc$dim[['altitude']]$len
  no_latitude <- source_nc$dim[['latitude']]$len
  no_longitude <- source_nc$dim[['longitude']]$len
  indices <- split > 1
  split[indices] <- unlist(split[indices]) + 1
  time_indices <- as.integer(seq(1, no_time, length.out = split$time))
  altitude_indices <-as.integer(seq(1, no_altitude, length.out = split$altitude))
  latitude_indices <- as.integer(seq(1, no_latitude, length.out = split$latitude))
  longitude_indices <- as.integer(seq(1, no_longitude,  length.out = split$longitude))
  no_time_extracts <-  ifelse(length(time_indices) == 1, 1, length(time_indices) - 1)
  no_altitude_extracts <-  ifelse(length(altitude_indices) == 1, 1, length(altitude_indices) - 1)
  no_latitude_extracts <-  ifelse(length(latitude_indices) == 1, 1, length(latitude_indices) - 1)
  no_longitude_extracts <-  ifelse(length(longitude_indices) == 1, 1, length(longitude_indices) - 1)
  no_extracts <- data.frame(time = no_time_extracts,
                            altitude = no_altitude_extracts,
                            latitude = no_latitude_extracts,
                            longitude = no_longitude_extracts
  )
  dim_indices <- data.frame(time = time_indices,
                            altitude = altitude_indices,
                            latitude = latitude_indices,
                            longitude = longitude_indices
  )
  return(list(no_extract = no_extracts, dim_indices = dim_indices))
}




create_nc_file <- function(data_info, fields, source_file, destination_file) {
  # Open the source netCDF file
  source_nc <- ncdf4::nc_open(source_file)
  # Copy dimensions
  source_dim <- list()
  for (dim_name in rev(names(source_nc$dim))) {
    source_dim[[dim_name]] <- ncdf4::ncdim_def(dim_name,
                                        source_nc$dim[[dim_name]]$units,
                                        source_nc$dim[[dim_name]]$vals
    )
  }
  # add fields from field list
  source_var <- list()
  #for (var_name in names(source_nc$var)) {
  for (var_name in fields) {
      print(var_name)
      prec <- data_info$alldata[[var_name]]$data_type[1]
      units_loc <- which(data_info$alldata[[var_name]]$attribute_name == 'units')
      units <- data_info$alldata[[var_name]]$value[units_loc]
      print(units)
      source_var[[var_name]]  <- ncdf4::ncvar_def(name = var_name,
                                         # units = source_nc$var[[var_name]]$units,
                                         units = units,
                                         dim = source_dim,
                                         missval = NA,
                                         #prec = source_nc$var[[var_name]]$prec
                                         prec = prec
    )
  }
  dest_nc <- ncdf4::nc_create(destination_file, vars = source_var, force_v4 = TRUE)
  # Close the netCDF files
  ncdf4::nc_close(source_nc)
  ncdf4::nc_close(dest_nc)
}
# ----------------------------------------
# Function to copy attributes
copy_attributes <- function(data_info, fields, source_file, destination_file ) {
  # Open the source and destination netCDF files
  source_nc <- ncdf4::nc_open(source_file)
  dest_nc <- ncdf4::nc_open(destination_file, write = TRUE)
  # Copy global attributes
  global_attrs <- ncdf4::ncatt_get(source_nc, 0)
  for (attr_name in names(global_attrs)) {
    ncdf4::ncatt_put(dest_nc, 0, attr_name, global_attrs[[attr_name]])
  }
  # copy for each var
  for (var_name in fields) {
    # var_attrs <- ncatt_get(source_nc, var_name)
    var_attrs <- data_info$alldata[[var_name]]$values
    var_attrs <- var_attrs[-1]
    for (attr in names(var_attrs)) {
      if(!(attr == '_FillValue')) {
        ncdf4::ncatt_put(dest_nc, var_name, attr, var_attrs[[attr]])
      }
    }
  }
  # Close the netCDF files
  ncdf4::nc_close(source_nc)
  ncdf4:: nc_close(dest_nc)
}


#############################
##  from package 'rerddap'
############################

field_handler <- function(x, y){
  x <- match.arg(x, c(y, "none", "all"), TRUE)
  if (length(x) == 1 && x == "all") {
    y
  } else if (all(x %in% y) || x == "none") {
    x
  }
}

check_dims <- function(dimargs, .info) {
  if (any(lengths(dimargs )!= 2)) {
    print("All coordinate bounds must be of length 2, even if same value")
    print("Present values are:")
    print(dimargs)
    stop("rerddap halted", call. = FALSE)
  }
  if (!all(names(dimargs) %in% dimvars(.info))) {
    stop(sprintf("Some input dimensions (%s) don't match those in dataset (%s)",
                 paste0(names(dimargs), collapse = ", "),
                 paste0(dimvars(.info), collapse = ", ")), call. = FALSE)
  }
}

check_lon_text <- function(dimargs) {
  if (!is.null(dimargs$longitude)) {
    if (any(sapply(dimargs$longitude, class) == "character")) {
      txt <- dimargs$longitude[sapply(dimargs$longitude, class) == "character"]
      if (!all(grepl("last", txt))) stop("Only text values allowed are 'last' & variants on that", call. = FALSE)
    }
  }
}

check_lat_text <- function(dimargs) {
  if (!is.null(dimargs$latitude)) {
    if (any(sapply(dimargs$latitude, class) == "character")) {
      txt <- dimargs$latitude[sapply(dimargs$latitude, class) == "character"]
      if (!all(grepl("last", txt))) stop("Only text values allowed are 'last' & variants on that", call. = FALSE)
    }
  }
}

is_lon_text <- function(dimargs) {
  if (!is.null(dimargs$longitude)) {
    any(sapply(dimargs$longitude, class) == "character")
  } else {
    FALSE
  }
}

is_lat_text <- function(dimargs) {
  if (!is.null(dimargs$latitude)) {
    any(sapply(dimargs$latitude, class) == "character")
  } else {
    FALSE
  }
}

check_time_range <- function(dimargs, x) {
  #  if(!class(dimargs$time) == 'character'){
  if(!is.character(dimargs$time)){
    print('time must be given as character strings')
    print('you are passing ', paste0(class(dimargs$time)))
    stop('rerddap halted', call. = FALSE)
  }
  global <- x$alldata$NC_GLOBAL
  tt <- global[ global$attribute_name %in%c('time_coverage_end','time_coverage_start'), "value", ]
  tt <- rev(tt)
  if (!('last' %in% dimargs$time)){
    if((dimargs$time[1] < tt[1]) | (dimargs$time[2] > tt[2])) {
      print('time bounds are out of range')
      print('You gave: ')
      print(dimargs$time)
      print("Dataset times are: ")
      print(tt)
      stop('rerddap halted', call. = FALSE)
    }
  }
}


check_lon_data_range <- function(dimargs, .info) {
  if (!is.null(dimargs$longitude)) {
    val <- .info$alldata$longitude[ .info$alldata$longitude$attribute_name == "actual_range", "value"]
    val2 <- as.numeric(strtrim(strsplit(val, ",")[[1]]))
    if (!is_lon_text(dimargs)) {
      if (max(dimargs$longitude) > max(val2) || min(dimargs$longitude) < min(val2)) {
        stop(sprintf("One or both longitude values (%s) outside data range (%s)",
                     paste0(dimargs$longitude, collapse = ", "),
                     paste0(val2, collapse = ", ")), call. = FALSE)
      }
    }
  }
}

check_lat_data_range <- function(dimargs, .info) {
  if (!is.null(dimargs$latitude)) {
    val <- .info$alldata$latitude[ .info$alldata$latitude$attribute_name == "actual_range", "value"]
    val2 <- as.numeric(strtrim(strsplit(val, ",")[[1]]))
    if (!is_lat_text(dimargs)) {
      if (max(dimargs$latitude) > max(val2) || min(dimargs$latitude) < min(val2)) {
        stop(sprintf("One or both latitude values (%s) outside data range (%s)",
                     paste0(dimargs$latitude, collapse = ", "),
                     paste0(val2, collapse = ", ")), call. = FALSE)
      }
    }
  }
}

fix_dims <- function(dimargs, .info) {
  for (i in seq_along(dimargs)) {
    tmp <- dimargs[[i]]
    nm <- names(dimargs[i])
    tmp <- grep("last+", tmp, value = TRUE, invert = TRUE)
    if (nm == "time") {
      tmp <- as.Date(tmp)
    }

    val <- .info$alldata[[nm]][ .info$alldata[[nm]]$attribute_name == "actual_range", "value"]
    temp_val <- strsplit(val, ",")[[1]]
    temp_val <- gsub("^\\s+|\\s+$", "", temp_val)
    val2 <- as.numeric(temp_val)
    # val2 <- as.numeric(strtrim(strsplit(val, ",")[[1]]))
    if (length(tmp) != 0) {
      if (which.min(val2) != which.min(tmp)) {
        dimargs[[i]] <- rev(dimargs[[i]])
      }
    }

    ## new
    # if (nm %in% c('latitude', 'longitude')) {
    if (nm != 'time') {
      z <- unlist(strsplit(.info$alldata[[nm]]$value[1], ","))
      spacing <- as.numeric(unlist(strsplit(z[3], "=")[[1]])[2])
      if ((!is.na(spacing)) & (spacing < 0)) {
        if (!(dimargs[[i]][1] > dimargs[[i]][2])) {
          dimargs[[i]] <- rev(dimargs[[i]])
        }
      }
    }
  }
  dimargs
}

parse_args <- function(.info, dim, s, dimargs, wname = FALSE){
  tmp <- if (dim %in% names(dimargs)) {
    dimargs[[dim]]
  } else {
    if (dim == "time") {
      times <- c(getvar(.info, "time_coverage_start"), getvar(.info, "time_coverage_end"))
      sprintf('[(%s):%s:(%s)]', times[1], s, times[2])
    } else {
      actrange <- foo(.info$alldata[[dim]], "actual_range")
      gsub("\\s+", "", strsplit(actrange, ",")[[1]])
    }
  }

  if (length(s) > 1) {
    if (!length(s) == length(dimvars(.info))) stop("Your stride vector must equal length of dimension variables", call. = FALSE)
    names(s) <- dimvars(.info)
    if (!wname) {
      sprintf('[(%s):%s:(%s)]', tmp[1], s[[dim]], tmp[2])
    } else {
      sprintf('%s[(%s):%s:(%s)]', dim, tmp[1], s[[dim]], tmp[2])
    }
  } else {
    if (!wname) {
      if (length(tmp) == 1) {
        tmp
      } else {
        sprintf('[(%s):%s:(%s)]', tmp[1], s, tmp[2])
      }
    } else {
      if (length(tmp) == 1) {
        tmp
      } else {
        sprintf('%s[(%s):%s:(%s)]', dim, tmp[1], s, tmp[2])
      }
    }
  }
}

getvar <- function(x, y){
  x$alldata$NC_GLOBAL[ x$alldata$NC_GLOBAL$attribute_name == y, "value"]
}

getvars <- function(x){
  vars <- names(x$alldata)
  vars[ !vars %in% c("NC_GLOBAL", "time", x$variables$variable_name) ]
}

getallvars <- function(x){
  vars <- names(x$alldata)
  vars[ !vars %in% "NC_GLOBAL" ]
}

dimvars <- function(x){
  vars <- names(x$alldata)
  vars[ !vars %in% c("NC_GLOBAL", x$variables$variable_name) ]
}

foo <- function(x, y){
  x[ x$attribute_name == y, "value"]
}

`%||%` <- function (x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

strextract <- function(str, pattern) regmatches(str, regexpr(pattern, str))

strtrim <- function(str) gsub("^\\s+|\\s+$", "", str)
