# CLAUDE.md

Guidance for Claude Code working **on** the rerddapUtils package source (drop this at the repo root of `rmendels/rerddapUtils`; it currently lives beside `SKILL.md` because no local checkout exists in this environment). rerddapUtils extends `rerddap` with season-restricted extraction, split/aggregate for oversized requests, and projected-coordinate conversion. R >= 4.4.

## Skill vs. this file

`SKILL.md` in this same directory is LLM-targeted guidance for *writing user code that calls rerddapUtils*. When helping a user *use* rerddapUtils, defer to the skill. **This file is for working on the package source itself.**

## Commands

```r
devtools::document()     # roxygen2 -> man/, NAMESPACE
devtools::load_all()     # load for interactive testing
```

```bash
R CMD build .
R CMD check --as-cran --no-manual rerddapUtils_*.tar.gz
```

**There is no `tests/testthat` directory in this repo** and no `.github/workflows` CI at all. This is the youngest and least-covered package in the family — roxygen `@examples` (guarded by an `httr::HEAD()` server-reachability check before running) are the only executable verification. Any nontrivial change to `griddap_split.R`'s recursive splitting logic in particular should be exercised manually against a real ERDDAP server before you consider it done; it's easy to construct a `request_split`/dimension combination that silently produces the wrong result.

## Codebase Shape (`R/`)

- `griddap_season.R` — `griddap_season()` + private `griddap_season_request()`, which loops one `rerddap::griddap()` call per calendar year in the requested time range and `rbind()`s the results. `fmt`/`store`/`read` are accepted (for interface parity with `rerddap::griddap()`) but ignored.
- `griddap_split.R` — the most structurally complex file in the package: `griddap_split()` → `split_griddap_request()` → `recursive_extract()` (recurses one level per dimension in `request_split`) → `partial_extract()` (issues one actual `rerddap::griddap()` call per leaf) → one of `aggregate_memory()`/`aggregate_duckdb()`/`aggregate_netcdf()` depending on `fmt`. `define_split()` and `get_dim_constraint()` compute the per-dimension index ranges before recursion starts. If you touch this file, trace a call by hand through `recursive_extract()` for at least a 2-dimension split before trusting a change.
- `proj_extract.R` / `conversion.R` — `latlon_to_xy()` / `xy_to_latlon()`. **The exported signature of `latlon_to_xy()` is `(dataInfo, longitude, latitude, xName = 'rows', yName = 'cols', crs = NULL)` — longitude before latitude.** The package's own README documents a different, stale signature (`latitude, longitude` order, different default arg names). Trust `conversion.R`, not the README, and fix the README if you're in the area.
- `estimate_griddap_size.R` — `estimate_griddap_size()` + `estimate_griddap_split_size()`. The first function is **near-byte-identical to `rerddap::estimate_griddap_size()`** (same name, same signature, independently maintained copy) — if you fix a bug or add a feature here, check the twin in `rerddap`'s `R/estimate_griddap_size.R` too. The two will keep drifting apart until one is made to depend on the other.
- `utils.R` — private re-implementation of rerddap-style internals (`dimvars`, `getvar`, `check_dims`, `check_time_range`, `fix_dims`, `parse_args`, etc.) plus this package's own additions: `extract_rerddap_call()` (builds the base griddap call list reused by both `griddap_season()` and `griddap_split()`), and the NetCDF-aggregation helpers `create_nc_file()`/`copy_attributes()`/`define_split_nc()` used by `griddap_split()`'s `fmt = 'nc'` path. Another independent copy of the rerddap-internals set — see the note in rerddap's `CLAUDE.md`.
- `rerddapUtils.R` — package-level roxygen doc block only, no logic.
- `iceInfo.R`, `wind_info.R`, `proj_extract.R` (doc half) — `data/*.rda` documentation stubs for bundled example objects (a projected-grid `info()` result, a `griddap()` result, and a wind dataset `info()` result) used across `@examples`.

## Style Convention: `cli::cli_warn()` + `return(NULL)`, Not `stop()`

`griddap_season()` and `griddap_split()` respond to a missing required argument (`season`, `request_split`) with `cli::cli_warn(...)` followed by `return(NULL)`, not `stop()`. This is a deliberate, existing convention — keep it consistent for new validation in these two functions (and check for `NULL` after calling them, not a `tryCatch`). Don't silently switch to `stop()` in one function while leaving the other on `cli_warn()`.

## Packaging Notes

- `Roxygen: list(markdown = TRUE)` in `DESCRIPTION` — docs use markdown roxygen syntax.
- `Config/roxygen2/version: 8.0.0.9000` — a roxygen2 dev version; regenerating docs with an older roxygen2 install may produce a large, noisy diff in `man/`/`NAMESPACE`.
- No formatter/linter config — match surrounding style by hand.
