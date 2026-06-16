---
layout: default
title: Accessing Planet data in R
---

*A 15-minute primer by the Osa Conservation science team.*

By the end of this you'll be able to pull Planet Forest Diligence values (above-ground carbon density, canopy cover, canopy height, and their associated error) for your own field sites in R - whether those sites are points (e.g. camera traps, bird stations) or polygons (e.g. restoration plots, conservation easements)! You don't permenantly download imagery - you'll only pull the summary statistics you need, so the workflow scales to hundreds of sites easily!

<img src="https://raw.githubusercontent.com/Osa-Conservation/Extracting_planet_data_to_survey_sites_in_R/refs/heads/main/figures/banner_figure.png" height="400"/>

The three R scripts (setup, points and polygons) live in [`the R/ folder`](https://github.com/Osa-Conservation/Extracting_planet_data_to_survey_sites_in_R/tree/main/R). This page walks an overview of the process.

------------------------------------------------------------------------

*Important caveat*: this code uses the Sentinal Process API, but the Statistical API is purpose-built for generating time series within polygons. It returns mean/stdev/percentiles per time interval directly. It is probably better than this template if you mainly want change-over-time - but the underlying mechanisms are pretty much the same!

------------------------------------------------------------------------

## Setup

The R code to set up your session leaves here: [`R/00_setup.R`](https://github.com/Osa-Conservation/Extracting_planet_data_to_survey_sites_in_R/blob/main/R/00_setup.R)

To get this running for uyour own sites or locations you will need:

### 1. A Sentinel Hub account with an OAuth client

Sign up at [sentinel-hub.com](https://www.sentinel-hub.com/). From the user settings page, create an *OAuth client*(go to <https://insights.planet.com/account/> then the OAuth client box and "Create new") then copy the `client_id` and `client_secret` somewhere safe. It is a good idea to set your credentials to expire after a certain time period just in-case they get exposed.

### 2. Credentials in `~/.Renviron` - *not* in your scripts

If you are going to work locally, ignore this. If you are going to work on github this is very important! Anything you commit to a GitHub repository is effectively public forever. Even if you are just going to work locally it is still a good idea to look after passwords and credentials.

Open `~/.Renviron` with:

``` r
usethis::edit_r_environ()
```

…and add two lines:

```         
SH_CLIENT_ID=your-client-id-here
SH_CLIENT_SECRET=your-client-secret-here
```

Save, restart R. The scripts associated here then read them with `Sys.getenv()`, so they never appear in code! The `.gitignore` excludes `.Renviron` so it can't accidentally be committed.

### 3. Packages

``` r
install.packages(c("httr", "sf", "terra", "dplyr", "tidyr", "ggplot2", "leaflet"))
```

That's it. Remember - run [`R/00_setup.R`](https:///github.com/Osa-Conservation/Extracting_planet_data_to_survey_sites_in_R/blob/main/R/00_setup.R) before you dive into the other scripts.

------------------------------------------------------------------------

## The workflow, in a nutshell:

<img src="https://raw.githubusercontent.com/Osa-Conservation/Extracting_planet_data_to_survey_sites_in_R/refs/heads/main/figures/workflow.png" height="400"/>

------------------------------------------------------------------------

## Authenticate (both Scripts)

The Sentinel Hub API uses OAuth2 client credentials. The code below wsaps your `client_id` and `client_secret` for a short-lived bearer token (\~1 hour), then attaches that token to every API call.

``` r
library(httr)

get_token <- function(client_id, client_secret) {
  response <- POST(
    "https://services.sentinel-hub.com/oauth/token",
    encode = "form",
    body   = list(grant_type    = "client_credentials",
                  client_id     = client_id,
                  client_secret = client_secret)
  )
  if (http_status(response)$category != "Success") stop("Authentication failed")
  content(response, "parsed")$access_token
}

bearer_token <- get_token(Sys.getenv("SH_CLIENT_ID"),
                          Sys.getenv("SH_CLIENT_SECRET"))
```

------------------------------------------------------------------------

## Points workflow (single values per location)

*Full script: [`R/01_points_workflow.R`](https://github.com/Osa-Conservation/Extracting_planet_data_to_survey_sites_in_R/blob/main/R/01_points_workflow.R)*

You should start with a .csv of you sites which looks something like this:

```         
id,lon,lat
OSA_01,-83.5810,8.3870
OSA_02,-83.5905,8.3920
...
```

Read it into R, specify its CRS, and buffer each point into a circular AOI. The buffer must happen in a projected CRS so the radius is in real metres - for the Osa Peninsula I use UTM 17N (EPSG:32617).

``` r
library(sf)

# Read in your locations
pts <- read.csv("data/example_points.csv")
pts_sf <- st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)

# Specify your buffer and create shapefiles
buffer_radius <- 50  # metres

aoi <- pts_sf |>
  st_transform(32617) |>     # UTM 17N — change for your region
  st_buffer(buffer_radius) |>
  st_transform(4326)

# Time window. Forest Carbon Diligence is annual; give a window that spans the
# year(s) you want - here we use 2024.
time_from <- "2024-01-01T00:00:00Z"
time_to   <- "2024-12-31T23:59:59Z"
```

**Check your survey locations!** If I had a penny for every time a collaborator gave me survey locations projecting on the wrong continent or in the ocean...

``` r
leaflet() |>   addProviderTiles(providers$Esri.WorldImagery) |>   addPolygons(data = aoi, color = "#FF6600", fillOpacity = 0.3, popup = aoi$id) 
```

<img src="https://raw.githubusercontent.com/Osa-Conservation/Extracting_planet_data_to_survey_sites_in_R/refs/heads/main/figures/points_check.png" height="400"/>

Looking good!

Each row of `aoi` is now a polygon ready to send to the API. The query function takes one polygon + one collection ID + a time window and returns the mean and error over that AOI:

``` r
sf_to_geojson_coords <- function(polygon_sf) {
  m <- st_coordinates(polygon_sf)[, 1:2]
  list(lapply(seq_len(nrow(m)), function(i) c(m[i, 1], m[i, 2])))
}

query_polygon_process <- function(polygon_sf, token, collection_id, bands,
                                  time_from, time_to) {

  # `bands` is a named vector, e.g. c(mean="ACD", lower="UC_Q05", upper="UC_Q95")
  # NULL entries (collections without uncertainty bands) are dropped silently.
  bands  <- bands[!vapply(bands, is.null, logical(1))]
  bands  <- bands[nzchar(bands)]
  n      <- length(bands)
  if (n == 0) stop("No bands requested.")

  # Build the evalscript arrays dynamically from the requested bands.
  inputs  <- paste0('"', c(unname(bands), "dataMask"), '"', collapse = ", ")
  returns <- paste0("sample.", unname(bands), collapse = ", ")
  nans    <- paste0(rep("NaN", n), collapse = ", ")

  evalscript <- sprintf('//VERSION=3
function setup() {
  return { input: [%s],
           output: { bands: %d, sampleType: "FLOAT32" } };
}
function evaluatePixel(sample) {
  if (sample.dataMask == 1) { return [%s]; }
  return [%s];
}', inputs, n, returns, nans)

  body <- list(
    input = list(
      bounds = list(
        geometry   = list(type = "Polygon",
                          coordinates = sf_to_geojson_coords(polygon_sf)),
        properties = list(crs = "http://www.opengis.net/def/crs/EPSG/0/4326")
      ),
      data = list(list(
        type = collection_id,
        dataFilter = list(timeRange = list(from = time_from, to = time_to))
      ))
    ),
    output = list(width = 64, height = 64,
                  responses = list(list(identifier = "default",
                                        format = list(type = "image/tiff")))),
    evalscript = evalscript
  )

  response <- POST("https://services.sentinel-hub.com/api/v1/process",
                   add_headers(Authorization  = paste("Bearer", token),
                               Accept         = "image/tiff",
                               `Content-Type` = "application/json"),
                   body = body, encode = "json")

  # One NA per requested band, named, so the caller can always bind columns.
  empty <- setNames(rep(NA_real_, n), names(bands))
  if (status_code(response) != 200) {
    message("Error: ", content(response, "text"))
    return(empty)
  }

  tmp <- tempfile(fileext = ".tif")
  writeBin(content(response, "raw"), tmp)
  r <- rast(tmp)
  r <- mask(r, vect(polygon_sf))   # hard-crop to the polygon (drop bbox corners)
  vals <- values(r)
  unlink(tmp)

  # After mask(), out-of-polygon pixels are NA and in-polygon no-data is NaN.
  # is.na() catches both, so we keep only genuine in-polygon values.
  if (!is.matrix(vals)) vals <- matrix(vals, ncol = 1)
  keep <- !is.na(vals[, 1])
  if (!any(keep)) return(empty)

  means <- colMeans(vals[keep, , drop = FALSE], na.rm = TRUE)
  setNames(means, names(bands))
}
```

Three things worth understanding in that block:

- **IMPORTANT POINT 1:** **`keep <- !is.na(vals[, 1])`** drops no-data pixels - this might bias the mean upward by excluding genuinely cleared land. Keep zeros unless you have a band-specific reason to drop them!
- **IMPORTANT POINT 2: `width = 64, height = 64`** asks the server to return a 64×64 raster covering the AOI's bounding box. For small AOIs this can oversample Forest Carbon's 30 m pixels, which is fine for a mean. For a strict native-resolution read, swap for `resx = 0.0003, resy = 0.0003`.

Then specify the products you want - swapping the ids for codes in your collection ([Data Collections ・ Planet Insights Platform](https://insights.planet.com/data/collections/#/?tab=planet)). **Note** if you want to create a new collection (e.g. if updated Forest Diligence variables are released) then use this tool - <https://insights.planet.com/data/subscriptions/new>.

<img src="https://raw.githubusercontent.com/Osa-Conservation/Extracting_planet_data_to_survey_sites_in_R/refs/heads/main/figures/band_selection.png" height="400"/>

``` r
collections <- list(
  biomass = list(
    id    = "byoc-96357063-a972-4410-a7a9-5e56105ae393",
    mean  = "ACD",
    lower = "UC_Q05",
    upper = "UC_Q95"
  ),
  canopy_cover = list(
    id    = "byoc-ed6d973a-7449-4721-bf8f-c465fc4382e4",
    mean  = "CC",
    lower = "UC_Q05",
    upper = "UC_Q95"
  ),
  canopy_height = list(
    id    = "byoc-dea673eb-421b-4e91-bd5c-7bbac6be022c",
    mean  = "CH",
    lower = "UC_Q05",
    upper = "UC_Q95"
  )
)


# The specify the loop to pull the results

results <- do.call(rbind, lapply(seq_len(nrow(aoi)), function(i) {
  cent <- st_coordinates(st_centroid(aoi[i, ]))
  row  <- data.frame(id = aoi$id[i], lon = cent[1,1], lat = cent[1,2])
  for (m in names(collections)) {
    col   <- collections[[m]]
    means <- query_polygon_process(
      polygon_sf = aoi[i, ], token = bearer_token,
      collection_id = col$id,
      bands = c(mean = col$mean, lower = col$lower, upper = col$upper),
      time_from = time_from,
      time_to   = time_to)
    # fan the named vector out into biomass_mean, biomass_lower, biomass_upper ...
    for (nm in names(means)) row[[paste0(m, "_", nm)]] <- unname(means[nm])
    Sys.sleep(0.2)
  }
  row
}))
```

One tidy row per site, one column per metric. Map it again to check it looks reasonable!

``` r
library(leaflet)
leaflet() |>
  addProviderTiles(providers$Esri.WorldImagery) |>
  addPolygons(data = aoi, color = "#FF6600", fillOpacity = 0.3) |>
  addCircleMarkers(data = results, ~lon, ~lat, radius = 5, color = "red")
```

<img src="https://raw.githubusercontent.com/Osa-Conservation/Extracting_planet_data_to_survey_sites_in_R/refs/heads/main/figures/points_biomass_output.png" height="400"/>

------------------------------------------------------------------------

## Step 3 — Polygon workflow with a time series

*Full script: [`R/02_polygon_workflow.R`](https://github.com/YOUR-GH-USERNAME/osa-planet-r-tutorial/blob/main/R/02_polygon_workflow.R)*

If you already have polygons (e.g. a shapefile of your plots), you skip buffering and feed them straight in. The same `query_polygon_process()` function works — only the loop changes, because now you want one value per plot **× year**.

``` r
plots_sf <- st_read("data/plots.shp") |> st_transform(4326)
plots_sf$id <- plots_sf$plot_id   # rename to a consistent column

# Cast MULTIPOLYGON → POLYGON if needed; the API accepts one polygon per call.
if (any(st_geometry_type(plots_sf) == "MULTIPOLYGON")) {
  plots_sf <- st_cast(plots_sf, "POLYGON")
}

years <- 2014:2024
time_periods <- data.frame(
  year = years,
  from = sprintf("%d-01-01T00:00:00Z", years),
  to   = sprintf("%d-12-31T23:59:59Z", years)
)

# Remember to plot your polygons before extracting the data!
```

Nested loop — plots × years × collections — produces a long data frame:

``` r
ts <- data.frame()
for (i in seq_len(nrow(plots_sf))) {
  for (t in seq_len(nrow(time_periods))) {
    row <- data.frame(plot_id = plots_sf$id[i], year = time_periods$year[t])
    for (m in names(collections)) {
      col   <- collections[[m]]
      means <- query_polygon_process(
        plots_sf[i, ], bearer_token,
        col$id,
        c(mean = col$mean, lower = col$lower, upper = col$upper),
        time_periods$from[t], time_periods$to[t])
      for (nm in names(means)) row[[paste0(m, "_", nm)]] <- unname(means[nm])
    }
    ts <- rbind(ts, row)
  }
}
```

Reshape and plot:

``` r
library(dplyr); library(tidyr); library(ggplot2)

# split each column name (e.g. biomass_mean) into a metric and a statistic,
# then widen so each row has mean/lower/upper for one plot x year x metric
ts_long <- ts |>
  pivot_longer(
    cols = -c(plot_id, year),
    names_to = c("metric", "stat"),
    names_pattern = "(.*)_(mean|lower|upper)"
  ) |>
  pivot_wider(names_from = stat, values_from = value)

ggplot(ts_long, aes(year, mean, colour = plot_id, fill = plot_id, group = plot_id)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, colour = NA) +
  geom_line() + geom_point() +
  facet_wrap(~ metric, scales = "free_y", ncol = 1) +
  theme_minimal()
```

[[[[[[NICE FIGURE]]]]]]]

For a quick visual of **change** between two years — useful for restoration plot reporting — compute Δbiomass and put it on a leaflet map with a diverging palette:

``` r
delta <- ts |>
  filter(year %in% c(2017, 2024)) |>
  select(plot_id, year, biomass_mean) |>
  pivot_wider(names_from = year, values_from = biomass_mean, names_prefix = "b") |>
  mutate(delta = b2024 - b2017)

plots_map <- plots_sf |> left_join(delta, by = c("id" = "plot_id"))

pal <- colorBin("RdYlGn",
                domain = plots_map$delta,
                bins = c(-Inf, -10, -5, 0, 5, 10, 15, 20, Inf))

leaflet(plots_map) |>
  addProviderTiles(providers$Esri.WorldImagery) |>
  addPolygons(fillColor = ~pal(delta), color = "black",
              weight = 1, fillOpacity = 0.7) |>
  addLegend(pal = pal, values = ~delta, title = "Δ Biomass (Mg/ha)")
```

[[[[[[[[[[[[[nice figure]]]]]]]]]]]

------------------------------------------------------------------------

## Steps to adapt this for your own work

1.  **Credentials** in `~/.Renviron`, never in the script.
2.  **Collection IDs** swapped to your own from <https://insights.planet.com/data/collections/>.
3.  **Source CRS** matches how your coordinates were recorded.
4.  **Buffer CRS - points only -** use a local UTM zone (true metres, not Web Mercator).
5.  **Time window** to match the annual layer(s) you want.
6.  **ID column** renamed to whatever the scripts expect (`id` for points, `plot_id` for polygons), or update the scripts.
