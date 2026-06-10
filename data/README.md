# `data/` — what goes here

This folder holds inputs (your sites and shapefiles) and outputs (CSVs of zonal stats).

## Inputs

| File | Used by | Notes |
|---|---|---|
| `example_points.csv` | `01_points_workflow.R` | Eight example sites in the Osa Peninsula. Replace with your own CSV — keep the columns `id`, `lon`, `lat`. |
| `plots.shp` (+ `.shx`, `.dbf`, `.prj`) | `02_polygon_workflow.R` | Your polygon shapefile. Must have a unique identifier column — set its name in the script via `id_column`. Not tracked in git by default; add it locally. |

## Outputs (written when the scripts run)

| File | Source | Notes |
|---|---|---|
| `example_points_results.csv` | `01_points_workflow.R` | One row per site with mean biomass / cover / height. |
| `plots_timeseries.csv` | `02_polygon_workflow.R` | One row per plot × year. |

## Coordinate reference system — read this once

The single biggest cause of "my points landed in the ocean" is mismatched CRS. Whatever projection your raw coordinates were recorded in (lon/lat WGS84, CRTM05, UTM…), set that **exact** CRS in `st_as_sf(..., crs = N)`. Only after that is correct should you transform to 4326.
