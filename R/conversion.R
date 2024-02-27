#' Convert Projected 'rerddap::griddap()' Coordinates to Latitude-Longitude
#'
#' This function converts the projected coordinates from a 'rerddap::griddap()' response
#' into geographical coordinates (latitude and longitude). It supports responses from
#' `griddap`, `rxtracto`, and `rxtracto3D` by detecting the response type and applying
#' the appropriate Coordinate Reference System (CRS) transformation.
#'
#' @param resp A response object from a call to `rerddap::griddap()`. It is expected to
#'   be of type `griddap_nc`, `rxtracto3D`, or `rxtractoTrack`, which contains the projected
#'   coordinate data along with metadata including the dataset's CRS.
#' @param yName The name of the variable in `resp` that represents the Y coordinate
#'   (typically latitude or northing). Defaults to 'cols'.
#' @param xName The name of the variable in `resp` that represents the X coordinate
#'   (typically longitude or easting). Defaults to 'rows'.
#' @param crs An optional CRS code to be used for the transformation. If not provided,
#'   the function attempts to automatically detect the CRS from the `resp` metadata. The
#'   CRS code should be a valid EPSG code (e.g., `4326` for WGS84) or a PROJ string.
#'
#' @return A matrix with two columns (`longitude`, `latitude`) containing the geographic
#'   coordinates corresponding to the projected coordinates in the input `resp`. Each row
#'   in the matrix corresponds to a point in `resp`.
#'
#' @export
#'
#' @examples
#' xgrid <- c(162500, 512500)
#' ygrid <- c(-437500,-637500)
#' xName <- 'xgrid'
#' yName <- 'ygrid'
#' myURL <- 'https://polarwatch.noaa.gov/erddap/'
#' myInfo <- rerddap::info('nsidcG02202v4sh1day', url = 'https://polarwatch.noaa.gov/erddap/')
#' proj_extract <- rerddap::griddap(myInfo,
#'                                  time = c('2023-06-30T00:00:00Z', '2023-06-30T00:00:00Z'),
#'                                  ygrid = ygrid,
#'                                  xgrid = xgrid,
#'                                  fields = 'cdr_seaice_conc',
#'                                  url = myURL
#'  )
#' test <- xy_to_latlon(proj_extract, yName = 'ygrid', xName = 'xgrid')
xy_to_latlon <- function (resp, yName = 'cols', xName = 'rows', crs = NULL) {
  # resp is call to rerddap::griddap() check that it is the right type
  url <- attributes(resp)$url
  datasetid <- attributes(resp)$datasetid
  base_loc <- stringr::str_locate(url, 'erddap')
  base_loc <- base_loc[1, 2]
  base_url <- substr(url, 1, (base_loc + 1))
  dataInfo <- rerddap::info(datasetid, url = base_url)
  proj_strings <- c('proj4string', 'proj_crs_code', 'proj4text', 'projection',
                    'grid_mapping_epsg_code','grid_mapping_proj4',
                    'grid_mapping_proj4_params', 'grid_mapping_proj4text',
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

#' Convert Latitude-Longitude to Projected Coordinates
#'
#' This function converts geographic coordinates (latitude and longitude) into
#' projected coordinates based on a specified Coordinate Reference System (CRS).
#' The function can automatically detect the CRS from provided metadata (`dataInfo`)
#' or use a specified CRS code. It is designed to work with datasets obtained
#' from `rerddap::griddap()` but can be used with any geographic data that requires
#' coordinate transformation.
#'
#' @param dataInfo Metadata object containing CRS information, typically obtained
#'   from a `rerddap::info()` call on a dataset. It should include global attributes
#'   that contain CRS information.
#' @param latitude Numeric vector of latitudes to be converted.
#' @param longitude Numeric vector of longitudes to be converted.
#' @param yName Name of the latitude coordinate in the output projection. Defaults
#'   to 'latitude'.
#' @param xName Name of the longitude coordinate in the output projection. Defaults
#'   to 'longitude'.
#' @param crs Optional. A character string specifying the CRS to use for the
#'   projection. This can be an EPSG code (e.g., 'EPSG:4326' for WGS84) or a PROJ
#'   string. If NULL, the function attempts to detect the CRS from `dataInfo`.
#'
#' @return A matrix with columns corresponding to the projected x and y coordinates
#'   (in the order specified by `xName` and `yName`). Each row in the matrix
#'   corresponds to a pair of input latitude and longitude values.
#'
#' @export
#'
#' @examples
#' myURL <- 'https://polarwatch.noaa.gov/erddap/'
#' myInfo <- rerddap::info('nsidcG02202v4sh1day', url = myURL)
#' latitude <- c(20, 30)
#' longitude <- c(-140, -130)
#' coords <- latlon_to_xy(myInfo, latitude, longitude)
latlon_to_xy <- function (dataInfo, latitude, longitude, yName = 'latitude', xName = 'longitude', crs = NULL) {
  proj_strings <- c('proj4string', 'proj_crs_code', 'proj4text', 'projection',
                    'grid_mapping_epsg_code','grid_mapping_proj4',
                    'grid_mapping_proj4_params', 'grid_mapping_proj4text',
                    'WKT')
  if (!is.null(crs)) {
    proj_crs_code = crs
  } else {
    crs_test <- intersect( unlist(proj_strings), unlist(dataInfo$alldata$NC_GLOBAL$attribute_name))
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
  # EPSG:4326 is basic lat-lon coordinates
  temp_df <- sf::st_as_sf(temp_df, coords = c("Lon", "Lat"), crs = 'EPSG:4326')
  # project data
  temp_df <- sf::st_transform(temp_df, crs = proj_crs_code)
  # get projection coordinates
  coordinates <- sf::st_coordinates(temp_df)
  return(coordinates)
}
