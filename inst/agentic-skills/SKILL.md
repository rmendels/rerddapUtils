---
name: rerddapUtils
description: Help users write correct R code with rerddapUtils to extend rerddap — restrict a griddap() request to a season of the year (griddap_season), split and aggregate a request too large for one call (griddap_split), convert between lat/lon and a dataset's projected coordinates (latlon_to_xy/xy_to_latlon), or estimate request/split size. Use when users call griddap_season(), griddap_split(), latlon_to_xy(), xy_to_latlon(), estimate_griddap_size(), or estimate_griddap_split_size() from rerddapUtils.
metadata:
  author: Roy Mendelssohn (@rmendels)
  version: "1.0"
license: CC0
---

rerddapUtils adds four capabilities `rerddap` deliberately leaves out: restricting a `griddap()` request to a recurring season, splitting and aggregating a request too large for one call, converting between lat/lon and a projected dataset's native grid coordinates, and estimating request/split sizes.

## Core Principle: Same Shape as griddap(), One Extra Argument

`griddap_season()` and `griddap_split()` mirror `rerddap::griddap()`'s signature almost exactly — same `datasetx`, `...` dimension args, `fields`, `stride` — plus one new argument each. Read them as "griddap() with a twist," not as unrelated functions.

```r
library(rerddapUtils)
library(rerddap)

dataInfo <- info('erdMBsstd1day')

# griddap_season(): same call as griddap(), plus `season`
season_extract <- griddap_season(dataInfo,
  time = c('2015-01-01', '2018-01-01'),
  latitude = c(20, 21), longitude = c(220, 221),
  fields = 'sst', season = c('03-01', '03-15'))
```

**Wrong pattern:** passing `store`, `read`, or (for `griddap_season`) `fmt` to these functions expecting the same behavior as `rerddap::griddap()` — all three are accepted but silently **ignored**. `griddap_season()` always returns an in-memory `griddap_nc` object; `griddap_split()`'s `fmt` means something different entirely (see below).

## griddap_season(): Restrict to a Recurring Time-of-Year Window

`season` is `c("MM-DD", "MM-DD")` — the day-of-year window to keep, applied to **every year** spanned by the `time` range. Internally this loops one `griddap()` call per year and row-binds the results; it is not a single ERDDAP request.

```r
season_extract <- griddap_season(dataInfo,
  time = c('2015-01-01', '2018-01-01'),
  latitude = c(20, 21), longitude = c(220, 221),
  fields = 'sst',
  season = c('03-01', '03-15'))   # only March 1-15 of each year 2015-2018
```

Omitting `season` doesn't error — it warns (`{.arg season} must be provided`) and returns `NULL`. Always check the return value isn't `NULL` before using it.

## griddap_split(): Split a Too-Large Request

For requests that would exceed ERDDAP's (or your machine's) size limits. `request_split` is a **named list**, one entry per dimension in the request, giving how many pieces to cut that dimension into (`1` = don't split it). The function recurses over every combination of splits, issues one `griddap()` call per piece, and aggregates.

```r
request_split <- list(time = 4, altitude = 1, latitude = 1, longitude = 1)

result <- griddap_split(dataInfo,
  time = c('2015-01-01', '2016-01-01'),
  latitude = c(20, 21), longitude = c(220, 221),
  fields = 'sst',
  request_split = request_split,
  fmt = 'nc')          # 'nc', 'duckdb', or 'memory' -- NOT rerddap's csv/nc/parquet
```

`fmt` here controls the **aggregation destination**, not the wire format:

| `fmt` | Result |
|---|---|
| `'memory'` | in-memory data.frame (like a normal `griddap()` csv result), returned directly |
| `'nc'` | pieces stitched into one NetCDF file; returns the file **path** |
| `'duckdb'` | pieces appended into a DuckDB table; returns the database file **path** |

`aggregate_file` sets where the `'nc'`/`'duckdb'` output is written; if omitted, a temp file is created. **If `aggregate_file` already exists, the function warns and returns `NULL` instead of overwriting it** — rename or delete the existing file first.

**Wrong pattern:** assuming `request_split` needs an entry only for the dimension you actually want split. It's built from every dimension present in the call's `dimVals`; leaving one out will fail rather than default it to `1`.

## Projected Datasets: latlon_to_xy() / xy_to_latlon()

Some ERDDAP datasets are stored on a projected grid (e.g. polar-stereographic sea ice data) rather than plain latitude/longitude. These two functions convert between geographic coordinates and that dataset's native grid coordinates, so you can build a `griddap()` request in the dataset's own coordinate system.

```r
# geographic -> projected: build griddap() request coordinates
data(iceInfo)   # example info() object for a projected dataset
coords <- latlon_to_xy(iceInfo, longitude = c(-170, -165), latitude = c(80, 85))
# coords columns are named by xName/yName (default 'rows'/'cols')

ice <- griddap(iceInfo,
  time = c('2026-05-18', '2026-05-18'),
  rows = coords[, 'rows'], cols = coords[, 'cols'],
  fields = 'IceThickness')

# projected -> geographic: interpret a griddap()/rxtracto()/rxtracto_3D() result
data(proj_extract)
latlon <- xy_to_latlon(proj_extract, crs = "EPSG:3413")
# returns a matrix with columns longitude, latitude
```

CRS resolution order: an explicit `crs` argument, else the dataset's own `NC_GLOBAL` metadata (`proj4text`, `grid_mapping_epsg_code`, `WKT`, etc.) is searched automatically. If `xy_to_latlon()` can't reach the ERDDAP server to look up dataset metadata (needed only when `crs` isn't supplied), it warns and returns `NULL`.

**Wrong pattern:** `latlon_to_xy(dataInfo, latitude, longitude)` — the actual parameter order is `(dataInfo, longitude, latitude, xName = 'rows', yName = 'cols', crs = NULL)` (**longitude before latitude**, matching typical (x, y) convention, not the lat-then-lon order used elsewhere in the ERDDAP ecosystem). Pass both by name to avoid mixing them up.

## Estimating Sizes

```r
size_est <- estimate_griddap_size(dataInfo,
  latitude = c(30, 50), longitude = c(-140, -110),
  time = c('2020-01-01', '2020-12-31'))

# then: how big would one piece of a 4-way time split be?
estimate_griddap_split_size(size_est, splits = list(time = 4))
```

`estimate_griddap_size()` is metadata-only (no network call) and returns its breakdown invisibly for scripting; `estimate_griddap_split_size()` takes that same object plus a `splits` named list (dims not listed default to a split count of 1) and reports the conservative (ceiling-rounded) per-split size.

**Wrong pattern:** `rerddap` (>= 1.3.0) ships its own `estimate_griddap_size()` with an identical name and near-identical signature. If both `rerddap` and `rerddapUtils` are loaded, whichever was attached **last** wins on an unqualified call — namespace-qualify (`rerddapUtils::estimate_griddap_size()`) if you need to be sure which one runs.

## Common Mistakes

| Mistake | Fix |
|---|---|
| Passing `store`/`read` (and for `griddap_season`, `fmt`) expecting rerddap's behavior | These are accepted but ignored by `griddap_season()`/`griddap_split()` — check the docs per function for what's actually honored |
| Treating `griddap_split()`'s `fmt` like rerddap's csv/nc/parquet format | It picks the aggregation target: `'memory'`, `'nc'`, or `'duckdb'` |
| Omitting an entry in `request_split` for a dimension used in the call | Every dimension in the request needs an entry (use `1` for "don't split") |
| Re-running `griddap_split()` with the same `aggregate_file` path | It refuses to overwrite an existing file — warns and returns `NULL` |
| Calling `latlon_to_xy(info, lat, lon)` positionally | Actual order is `(dataInfo, longitude, latitude, ...)` — pass `latitude =`/`longitude =` by name |
| Assuming `griddap_season()`/`griddap_split()` failures raise errors | Missing `season`/`request_split` produce a `cli::cli_warn()` and return `NULL`, not a `stop()` — always check for `NULL` |
| Calling unqualified `estimate_griddap_size()` with both `rerddap` and `rerddapUtils` loaded | Same function name in both packages — last-attached wins; qualify with `rerddapUtils::` if it matters |

## Quick Reference

| Task | Function |
|---|---|
| Restrict a `griddap()` extract to a day-of-year window across years | `griddap_season()` |
| Split a too-large `griddap()` request and aggregate the pieces | `griddap_split()` |
| Geographic (lon/lat) → dataset's projected grid coordinates | `latlon_to_xy()` |
| Dataset's projected grid coordinates → geographic (lon/lat) | `xy_to_latlon()` |
| Estimate a `griddap()` request's size, no network call | `estimate_griddap_size()` |
| Estimate the size of one piece of a split request | `estimate_griddap_split_size()` |
