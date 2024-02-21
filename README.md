
# rerddapUtils (Version 0.0.0.9000)

******
THIS VERSION IS FOR TESTING ONLY
DO NOT USE IN IMPORTANT PROJECTS
SOME TESTS FOR BAD INPUT STILL NEED TO BE ADDED
******


 `rerddapUtils` is an [R]{style="color:red"} package is a set of four functions desigened to work with and extend the `rerddap` package to provide capabilities requested by users that meet specialized needs which are better not being included in the `rerddao` package.
 
 The first function is:
 
- `griddap_season <- function(datasetx, ..., fields = 'all', stride = 1, season = NULL, fmt = "nc", url = eurl(), store = disk(), read = TRUE, callopts = list())`
                          
which will only extract data for the period during the year defined by 'season'.  The second function is:
  
  
- `griddap_split <- function(datasetx, ..., fields = 'all', stride = 1, request_split = NULL, fmt = "nc", url = eurl(), store = disk(), read = TRUE, callopts = list(), aggregate_file = NULL)`
              
which is designed to split a very large request into pieces defined by 'request_split' and then aggregate the parts either in memory,  in a duckdb database or in a netcdf4 file.  This allows for requests larger than 2GB to be made in an appropriate manner.
 
Note that `griddap_season()` and `griddap_split()` are designed to have basically the same interface as `rerddao::griddap()`,  except for one or two  extra function arguments and some arguments that are interpted differently. In both functions the argument 'store = disk()' is ignored.  In `griddap_season` the 'fmt' argument is ignored  while 'season' defines the days during the year to make the extract.

In  `griddap_split` the argument 'fmt' defines whether the aggregate download should be stored in memory as a dataframe,  in a duckdb database file,  or in a netcdf file.  For the duckdb and nectdf options, 'aggregate_file' lets you define where to write the file,  if not defined it will set up a temporary file following proper [R]{style="color:red"} guidelines and in both cases the path to the file will be returned.

The other two functions are designed to make it easier to work with projected datasets:

- `latlon_to_xy <- function (dataInfo, latitude, longitude, yName = 'latitude', xName = 'longitude', crs = NULL) `

which for a given dataset will convert a latitude and longitude request into the projected coordiantes to be used in `rerddao::griddap()`, while the fourth function:

- `xy_to_latlon <- function (resp, yName = 'cols', xName = 'rows', crs = NULL)`

does the reverse,  given an `rerddao::griddap()` extract will convert the projected coordinates into latitiude and longitude values.

This vignette assumes familiarity with the various 'rerddap' functions and how to use them.  More information about the various 'rerddap' functions can be found in the documentation for that package.

## Installation

You can install the development version of rerddapUtils like so:

``` r
remotes::install_github("rmendels/rerddapUtils"")
```

More detail and examples can be found in the Vignette.

