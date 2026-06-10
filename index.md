---
layout: default
title: Accessing Planet data in R
---

# Accessing Planet data in R

*A 15-minute primer for the Osa Conservation science team.*

By the end of this you'll know how to pull **Planet Forest Carbon Diligence** values — aboveground carbon density, canopy cover, canopy height — for your own field sites, whether those sites are points (e.g. camera traps, bird stations) or polygons (e.g. restoration plots, conservation easements). You won't download imagery; you'll only pull the summary statistics you need, so the workflow scales to hundreds of sites without flooding your disk.

The full code lives in [`R/`](https://github.com/YOUR-GH-USERNAME/osa-planet-r-tutorial/tree/main/R). This page walks through the logic.

---

## Why this exists

Field-based ecology asks the same question repeatedly: *what does the forest look like at my sites?* Traditionally that meant either (a) buying imagery and processing it yourself, or (b) querying Google Earth Engine through Python.

Planet's Forest Carbon Diligence collection — delivered through the Sentinel Hub API — gives you a third option that fits naturally into an R workflow:

- **Annual** rasters of biomass (ACD), canopy cover (CC), canopy height (CH) for the whole tropics.
- A **Process API** that lets you request just the pixels covering your AOI.
- No image downloads needed if all you want is a zonal summary.

That said, this is one workflow among several, with trade-offs worth knowing about upfront:

- **Sentinel Hub Statistical API** — purpose-built for time series over a polygon. Returns mean/stdev/percentiles per time interval directly. Better than this template if you mainly want change-over-time per AOI; uses the same auth + collections.
- **Google Earth Engine** — vast catalogue beyond Planet, free for non-commercial use, but requires Python or the JS code editor and a separate account.
- **Manual download + local processing** — most control, most disk and processing time.

This tutorial uses the **Process API** because it gives full control over how the reducer works (masking, no-data handling, custom thresholds) and slots into existing R workflows with minimal new dependencies.

---

## Before the session — 5 minutes of setup

You'll need three things:

### 1. A Sentinel Hub account with an OAuth client

Sign up at [sentinel-hub.com](https://www.sentinel-hub.com/). From the user settings page, create an **OAuth client** and copy the `client_id` and `client_secret` somewhere safe.

### 2. Credentials in `~/.Renviron` — *not* in your scripts

This matters. Anything you commit to GitHub is effectively public forever. Open `~/.Renviron` with:

```r
usethis::edit_r_environ()
```

…and add two lines:

```
SH_CLIENT_ID=your-client-id-here
SH_CLIENT_SECRET=your-client-secret-here
```

Save, restart R. The scripts here read them with `Sys.getenv()`, so they never appear in code. The `.gitignore` excludes `.Renviron` so it can't accidentally be committed.

### 3. Packages

```r
install.packages(c("httr", "sf", "terra", "dplyr", "tidyr", "ggplot2", "leaflet"))
```

That's it. Run [`R/00_setup.R`](https://github.com/YOUR-GH-USERNAME/osa-planet-r-tutorial/blob/main/R/00_setup.R) to confirm everything works before the session.

---

## The whole workflow, in one picture

```
         your sites (points or polygons)
                      │
                      ▼
            ┌─────────────────────┐
            │  buffer if points   │  ← skip for polygon shapefiles
            └──────────┬──────────┘
                       ▼
        ┌──────────────────────────────┐
        │  Sentinel Hub Process API    │
        │  request a small TIFF per    │
        │  AOI × collection × period   │
        └──────────────┬───────────────┘
                       ▼
            ┌──────────────────────┐
            │  read with terra,    │
            │  drop NaN, take mean │
            └──────────┬───────────┘
                       ▼
              tidy data frame
              one row per site (× year)
                       ▼
              ggplot / leaflet
```

Everything from here on is just R plumbing connecting those boxes.

---

## Step 1 — Authenticate

The Sentinel Hub API uses OAuth2 client credentials. You swap your `client_id` and `client_secret` for a short-lived bearer token (~1 hour), then attach that token to every API call.

```r
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

If you're doing a long run (hundreds of sites), the token may expire mid-run. Easiest fix: call `get_token()` again periodically, or wrap the query so a `401` triggers a refresh.

---

## Step 2 — Points workflow

*Full script: [`R/01_points_workflow.R`](https://github.com/YOUR-GH-USERNAME/osa-planet-r-tutorial/blob/main/R/01_points_workflow.R)*

You have a CSV like this:

```
id,lon,lat
OSA_01,-83.5810,8.3870
OSA_02,-83.5905,8.3920
...
```

Read it, declare its CRS, and buffer each point into a circular AOI. The buffer must happen in a **projected** CRS so the radius is in real metres — for the Osa Peninsula, UTM 17N (EPSG:32617).

```r
library(sf)

pts <- read.csv("data/example_points.csv")
pts_sf <- st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)

buffer_radius <- 100  # metres

aoi <- pts_sf |>
  st_transform(32617) |>     # UTM 17N — change for your region
  st_buffer(buffer_radius) |>
  st_transform(4326)
```

Each row of `aoi` is now a polygon ready to send to the API. The query function takes one polygon + one collection ID + a time window and returns the mean over that AOI:

```r
sf_to_geojson_coords <- function(polygon_sf) {
  m <- st_coordinates(polygon_sf)[, 1:2]
  list(lapply(seq_len(nrow(m)), function(i) c(m[i, 1], m[i, 2])))
}

query_polygon_process <- function(polygon_sf, token, collection_id, band_name,
                                  time_from, time_to) {
  evalscript <- sprintf('//VERSION=3
function setup() {
  return { input: ["%s", "dataMask"],
           output: { bands: 1, sampleType: "FLOAT32" } };
}
function evaluatePixel(sample) {
  if (sample.dataMask == 1) { return [sample.%s]; }
  return [NaN];
}', band_name, band_name)

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

  if (status_code(response) == 200) {
    tmp <- tempfile(fileext = ".tif")
    writeBin(content(response, "raw"), tmp)
    vals <- values(rast(tmp))
    vals <- vals[!is.nan(vals)]   # keep zeros, drop only masked pixels
    unlink(tmp)
    if (length(vals) > 0) return(mean(vals, na.rm = TRUE))
  }
  NA_real_
}
```

Three things worth understanding in that block:

- **The `evalscript`** is a small JavaScript snippet that runs on Sentinel Hub's servers, on every pixel of every relevant scene. It returns the band value where `dataMask == 1` and `NaN` otherwise.
- **`width = 64, height = 64`** asks the server to return a 64×64 raster covering the AOI's bounding box. For small AOIs this oversamples Forest Carbon's 30 m pixels, which is fine for a mean. For a strict native-resolution read, swap for `resx = 0.0003, resy = 0.0003`.
- **`vals[!is.nan(vals)]`** drops masked / out-of-footprint pixels. Earlier versions of this template also dropped zeros, but that biases the mean upward by silently excluding genuinely cleared land. Keep zeros unless you have a band-specific reason to drop them.

Then loop:

```r
collections <- list(
  biomass       = list(id = "byoc-XXXXXXXX-...", band = "ACD"),
  canopy_cover  = list(id = "byoc-XXXXXXXX-...", band = "CC"),
  canopy_height = list(id = "byoc-XXXXXXXX-...", band = "CH")
)

results <- do.call(rbind, lapply(seq_len(nrow(aoi)), function(i) {
  cent <- st_coordinates(st_centroid(aoi[i, ]))
  row  <- data.frame(id = aoi$id[i], lon = cent[1,1], lat = cent[1,2])
  for (m in names(collections)) {
    col <- collections[[m]]
    row[[paste0(m, "_mean")]] <- query_polygon_process(
      polygon_sf = aoi[i, ], token = bearer_token,
      collection_id = col$id, band_name = col$band,
      time_from = "2023-01-01T00:00:00Z",
      time_to   = "2023-12-31T23:59:59Z")
    Sys.sleep(0.2)
  }
  row
}))
```

One tidy row per site, one column per metric. Map it to sanity-check:

```r
library(leaflet)
leaflet() |>
  addProviderTiles(providers$Esri.WorldImagery) |>
  addPolygons(data = aoi, color = "#FF6600", fillOpacity = 0.3) |>
  addCircleMarkers(data = results, ~lon, ~lat, radius = 5, color = "red")
```

**Always do this** — a single wrong CRS or transposed lon/lat jumps out instantly on satellite imagery.

---

## Step 3 — Polygon workflow with a time series

*Full script: [`R/02_polygon_workflow.R`](https://github.com/YOUR-GH-USERNAME/osa-planet-r-tutorial/blob/main/R/02_polygon_workflow.R)*

If you already have polygons (a shapefile of plots), you skip buffering and feed them straight in. The same `query_polygon_process()` function works — only the loop changes, because now you want one value per plot **× year**.

```r
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
```

Nested loop — plots × years × collections — produces a long data frame:

```r
ts <- data.frame()
for (i in seq_len(nrow(plots_sf))) {
  for (t in seq_len(nrow(time_periods))) {
    row <- data.frame(plot_id = plots_sf$id[i], year = time_periods$year[t])
    for (m in names(collections)) {
      col <- collections[[m]]
      row[[m]] <- query_polygon_process(
        plots_sf[i, ], bearer_token,
        col$id, col$band,
        time_periods$from[t], time_periods$to[t])
    }
    ts <- rbind(ts, row)
  }
}
```

Reshape and plot:

```r
library(dplyr); library(tidyr); library(ggplot2)

ts_long <- ts |>
  pivot_longer(c(biomass, canopy_cover, canopy_height),
               names_to = "metric", values_to = "value")

ggplot(ts_long, aes(year, value, colour = plot_id, group = plot_id)) +
  geom_line() + geom_point() +
  facet_wrap(~ metric, scales = "free_y", ncol = 1) +
  theme_minimal()
```

For a quick visual of **change** between two years — useful for restoration plot reporting — compute Δbiomass and put it on a leaflet map with a diverging palette:

```r
delta <- ts |>
  filter(year %in% c(2017, 2024)) |>
  pivot_wider(names_from = year, values_from = biomass, names_prefix = "b") |>
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

---

## Adapting this for your own work — quick checklist

1. **Credentials** in `~/.Renviron`, never in scripts.
2. **Collection IDs** swapped to your own from <https://insights.planet.com/data/collections/>.
3. **Source CRS** matches how your coordinates were recorded.
4. **Buffer CRS** is a local UTM zone (true metres, not Web Mercator).
5. **Time window** matches the annual layer(s) you want.
6. **ID column** renamed to whatever the scripts expect (`id` for points, `plot_id` for polygons), or update the scripts.

---

## Going further

A few obvious extensions, in roughly increasing effort:

- **Statistical API instead of Process API** for cleaner multi-year time series — the server does the reducer, you just parse JSON.
- **Strict polygon mask** before averaging. Right now `terra::values()` includes bounding-box pixels not fully covered by the polygon; `terra::mask()` against the AOI fixes this if your sites are small.
- **Retry on token expiry** — wrap `query_polygon_process` so a `401` re-authenticates and retries.
- **Parallelise across sites** with `future.apply::future_lapply()` to cut runtime on hundreds of plots.
- **Add a Sentinel-2 NDVI workflow** alongside Forest Carbon for a finer-temporal signal between annual updates.

If you build something on top, please send a PR or open an issue — happy to add additional recipes here.
