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
#' @param crs An optional CRS code to be used for the transformation.
#'   If a CRS is found in the  `resp` metadata this is ignored.
#'   CRS code should be a valid EPSG code (e.g., `4326` for WGS84) or a PROJ string.
#' @return A matrix with two columns (`longitude`, `latitude`) containing the geographic
#'   coordinates corresponding to the projected coordinates in the input `resp`. Each row
#'   in the matrix corresponds to a point in `resp`.
#'
#' @export
#'
#' @examples
#' rows <- c( -889533.8, -469356.9)
#' cols <- c(622858.3, 270983.4)
#' xName <- 'xgrid'
#' yName <- 'ygrid'
#' myURL <- 'https://polarwatch.noaa.gov/erddap/'
#' myInfo <- rerddap::info('noaacwVIIRSn20icethickNP06Daily', url = myURL)
#' proj_extract <- rerddap::griddap(myInfo,
#'                                  time = c('2023-01-30T00:00:00Z', '2023-01-30T00:00:00Z'),
#'                                  rows = rows,
#'                                  cols = cols,
#'                                  fields = 'IceThickness',
#'                                  url = myURL
#'  )
#' test <- xy_to_latlon(proj_extract)
xy_to_latlon <- function (resp, yName = 'cols', xName = 'rows', crs = NULL) {
  # Validate resp is from rerddap and extract necessary attributes
  if (!inherits(resp, c('griddap_nc', 'rxtracto', 'rxtracto3D', 'rxtractoTrack'))) {
    stop("The response object is not recognized. Please provide a valid 'griddap', 'rxtracto', 'rxtracto3D', or 'rxtractoTrack' object.")
  }

  url <- attributes(resp)$url
  datasetid <- attributes(resp)$datasetid
  base_loc <- stringr::str_locate(url, 'erddap')[1, 2]
  base_url <- substr(url, 1, (base_loc + 1))
  dataInfo <- rerddap::info(datasetid, url = base_url)

  proj_strings <- c('proj4text', 'projection', 'proj4string', 'grid_mapping_epsg_code',
                    'WKT',  'proj_crs_code',
                    'grid_mapping_epsg_code', 'grid_mapping_proj4',
                    'grid_mapping_proj4_params', 'grid_mapping_proj4text')
  # if crs is in extract use that
  crs_test <- intersect(proj_strings, dataInfo$alldata$NC_GLOBAL$attribute_name)
  if (length(crs_test) == 0) {
    # check if a crs is given
    if (!is.null(crs)) {
      proj_crs_code <- crs
    } else {
      print('Could not find CRS information in the dataset and a CRS was not given')
      stop('Please provide CRS information in the function call.')
    }
  } else {
    proj_crs_code_index <- match(crs_test, dataInfo$alldata$NC_GLOBAL$attribute_name)
    proj_crs_code <- dataInfo$alldata$NC_GLOBAL$value[proj_crs_code_index]
  }
  temp_df <- extract_grid_data(resp, xName, yName)

  # Convert grid data to spatial data frame and transform to lat-lon
  coordinates <- prepare_and_transform(temp_df, proj_crs_code[1],  'EPSG:4326')
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
#' myInfo <- rerddap::info('noaacwVIIRSn20icethickNP06Daily', url = myURL)
#' latitude <- c( 80., 85.)
#' longitude <- c(-170., -165)
#' coords <- latlon_to_xy(myInfo,  longitude, latitude)
latlon_to_xy <- function (dataInfo, longitude, latitude,  xName = 'rows', yName = 'cols', crs = NULL) {
  proj_strings <- c('proj4text', 'projection', 'proj4string', 'grid_mapping_epsg_code',
                    'WKT',  'proj_crs_code',
                    'grid_mapping_epsg_code', 'grid_mapping_proj4',
                    'grid_mapping_proj4_params', 'grid_mapping_proj4text')

  # put lat-lon values into dataframe
  temp_df <- data.frame(longitude = longitude,  latitude = latitude)
  crs_given <- !is.null(crs)
  crs_test <- intersect(unlist(proj_strings), unlist(dataInfo$alldata$NC_GLOBAL$attribute_name))
  crs_in_file <- length(crs_test) > 0

  # Initialize proj_crs_code
  proj_crs_code <- NULL

  # Determine CRS code from file if available
  if (crs_in_file) {
    proj_crs_code_index <- match(crs_test, dataInfo$alldata$NC_GLOBAL$attribute_name)
    proj_crs_code <- dataInfo$alldata$NC_GLOBAL$value[proj_crs_code_index]
  }

  # Handle different cases
  if (!crs_in_file && !crs_given) {
    print("No CRS in file and no CRS given")
    print("Look in dataset metadata for CRS information")
    stop("Missing CRS information")
  } else if (crs_in_file && !crs_given) {
    # CRS in file, no CRS given
    coordinates <- prepare_and_transform(temp_df, 'EPSG:4326', proj_crs_code[1])
  } else if (crs_in_file && crs_given) {
    # CRS in file, CRS given
    crs_agree <- crs %in% proj_crs_code
    if (!crs_agree) {
      warning("The CRS given does not agree with the CRS in the file")
      warning(paste("The given CRS is", crs, "- the file CRS is", proj_crs_code[1]))
      warning('the file CRS will be used')
    }
    coordinates <- prepare_and_transform(temp_df, 'EPSG:4326', proj_crs_code[1])
  } else if (!crs_in_file && crs_given) {
    # No CRS in file, CRS given
    coordinates <- prepare_and_transform(temp_df, 'EPSG:4326', crs)
  }
  dimnames(coordinates)[[2]] <- c(xName, yName)
  return(coordinates)
}

# Extract grid data based on the type of response object
extract_grid_data <- function(resp, xName, yName) {
  if (inherits(resp, 'griddap_nc')) {
    data.frame(xgrid = resp$data[[xName]], ygrid = resp$data[[yName]])
  } else {
    data.frame(xgrid = resp[[xName]], ygrid = resp[[yName]])
  }
}


prepare_and_transform <- function(data_coords, crs_code_old, crs_code_new) {
  # Step 1: Convert the data frame to a simple feature (sf) object
  dimNames <- names(data_coords)
  temp_df <- sf::st_as_sf(data_coords, coords = c(dimNames[1], dimNames[2]), crs = crs_code_old)
  # Step 2: Transform the coordinates to
  temp_df <- sf::st_transform(temp_df, crs = crs_code_new)
  # Step 3: Extract the transformed coordinates
  transformed_coords <- sf::st_coordinates(temp_df)
  return(transformed_coords)
}
