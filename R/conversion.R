xy_to_latlon <- function (resp, yName = 'cols', xName = 'rows', crs = NULL) {
  # resp is call to rerddap::griddap() check that it is the right type
  url <- attributes(resp)$url
  datasetid <- attributes(resp)$datasetid
  base_loc <- stringr::str_locate(url, 'erddap')
  base_loc <- base_loc[1, 2]
  base_url <- substr(url, 1, (base_loc + 1))
  dataset_info <- rerddap::info(datasetid, url = base_url)
  proj_strings <- c('proj4string', 'proj_crs_code', 'proj4text', 'grid_mapping_epsg_code',
                    'grid_mapping_proj4', 'grid_mapping_proj4_params', 'grid_mapping_proj4text',
                    'WKT')

  if (!is.null(crs)) {
    proj_crs_code = crs
  } else {
    crs_test <- intersect( proj_strings, dataInfo$alldata$NC_GLOBAL$attribute_name)
    if (length(crs_test) == 0) {
      print('Could not find any of folowing fields for crs information')
      print( proj_strings)
      stop('please find crs info and provide in function call')
    }  else {
      proj_crs_code_index <- which(dataInfo$alldata$NC_GLOBAL$attribute_name == crs_test[1] )
      proj_crs_code <- dataInfo$alldata$NC_GLOBAL$value[proj_crs_code_index]
    }
  }
  # where the data reside depend on whether griddap, rxtracto, rxtracto3D
  if (inherits(resp, 'griddap_nc')) {
    temp_df <- data.frame(xgrid = resp$data[[xName]], ygrid = resp$data[[yName]])
  }  else if (inherits(resp, 'rxtracto3D')) {
       temp_df <- data.frame(xgrid = resp[[xName]], ygrid = resp[[yName]])
  }  else if (inherits(resp, 'rxtractoTrack')) {
       temp_df <- data.frame(xgrid = resp[[xName]], ygrid = resp[[yName]])
  }  else {
       print('the response does not appear to be from  any of')
       print('griddap, rxtracto, rxtracto_3D')
       stop('double-check your function call')
  }

  temp_df <- sf::st_as_sf(temp_df, coords = c('xgrid', 'ygrid'), crs = proj_crs_code)
  temp_df <- sf::st_transform(temp_df, crs = 4326)
  coordinates <- sf::st_coordinates(temp_df)
  dimnames(coordinates)[[2]] <- c('longitude', 'latitude')
  coordinates
  }

latlon_to_xy <- function (dataInfo, latitude, longitude, yName = 'latitude', xName = 'longitude', crs = NULL) {
  proj_strings <- c('proj4string', 'proj_crs_code', 'proj4text', 'grid_mapping_epsg_code',
                    'grid_mapping_proj4', 'grid_mapping_proj4_params', 'grid_mapping_proj4text',
                    'WKT')

  if (!is.null(crs)) {
    proj_crs_code = crs
  } else {
    crs_test <- intersect( proj_strings, dataInfo$alldata$NC_GLOBAL$attribute_name)
    if (length(crs_test) == 0) {
      print('Could not find any of folowing fields for crs information')
      print( proj_strings)
      stop('please find crs info and provide in function call')
    }  else {
      proj_crs_code_index <- which(dataInfo$alldata$NC_GLOBAL$attribute_name == crs_test[1] )
      proj_crs_code <- dataInfo$alldata$NC_GLOBAL$value[proj_crs_code_index]
    }
  }
  temp_df <- data.frame(Lat = latitude, Lon = longitude)
  # transform PB_Argos_subset to sf object
  # EPSG:4326 is basic lat-lon coordinates
  temp_df <- sf::st_as_sf(temp_df, coords = c("Lon", "Lat"), crs = 'EPSG:4326')
  # project data
  temp_df <- sf::st_transform(temp_df, crs = proj_crs_code)
  # get projection coordinates
  coordinates <- sf::st_coordinates(temp_df)
  return(coordinates)
}
