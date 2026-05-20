## New Submission

This is being resubmitted because of the problems with Fedora, and only Fedora.
This should fix the problems. Note the error said:

 test <- xy_to_latlon(proj_extract)
  Error: Proxy Error
  
proj_extract is a dataset stored in the package,  I have made the loading of the data
explicit.

## Test environments
* local OS X install, R 4.6.0 
* macOS-builder
* win-builder (devel and release)
* r-universe all platforms.

## R CMD check results

All OK

