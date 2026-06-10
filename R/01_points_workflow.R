# =============================================================================
# 01_points_workflow.R
#
# Workflow for POINT data:
#   1. Read a CSV of site coordinates.
#   2. Buffer each point into a circular AOI (in metres).
#   3. Query Planet Forest Carbon Diligence layers via the Sentinel Hub
#      Process API and compute the mean over each AOI.
#   4. Save to CSV and plot on a leaflet map.
#
# Assumes ~/.Renviron has SH_CLIENT_ID and SH_CLIENT_SECRET set.
# =============================================================================

library(httr)
library(sf)
library(terra)
library(leaflet)

# -------------------------- 1. CONFIG ----------------------------------------

# Buffer radius around each point (metres). Pick something ecologically
# meaningful — e.g. detection radius for camera traps, transect width, etc.
buffer_radius <- 50

# Time window. Forest Carbon Diligence is annual; give a window that spans the
# year(s) you want.
time_from <- "2024-01-01T00:00:00Z"
time_to   <- "2024-12-31T23:59:59Z"

# Collection ID's to query. Replace these BYOC IDs with your own from
# https://insights.planet.com/data/collections/ (TPDI tab).
collections <- list(
  biomass       = list(id = "byoc-96357063-a972-4410-a7a9-5e56105ae393", band = "ACD"),
  canopy_cover  = list(id = "byoc-ed6d973a-7449-4721-bf8f-c465fc4382e4", band = "CC"),
  canopy_height = list(id = "byoc-dea673eb-421b-4e91-bd5c-7bbac6be022c", band = "CH")
)

# -------------------------- 2. AUTH ------------------------------------------

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

# -------------------------- 3. READ POINTS -----------------------------------

pts <- read.csv("data/example_points.csv")

# IMPORTANT: set `crs =` to the CRS the coordinates were recorded in.
# If your CSV is already lon/lat: crs = 4326.
# If it's in a local projected system (e.g. CR's CRTM05 = 5367), use that and
# transform to 4326 below.
pts_sf <- st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)

# -------------------------- 4. BUFFER ----------------------------------------
# Buffer in a *projected* CRS so the radius is in real metres.
# UTM 17N (EPSG:32617) covers the Osa Peninsula. Pick the right UTM zone for
# wherever your sites are.

aoi <- pts_sf |>
  st_transform(32617) |>
  st_buffer(buffer_radius) |>
  st_transform(4326)

aoi$id <- pts_sf$id   # carry the identifier through

# -------------------------- 5. HELPERS ---------------------------------------

sf_to_geojson_coords <- function(polygon_sf) {
  m <- st_coordinates(polygon_sf)[, 1:2]
  list(lapply(seq_len(nrow(m)), function(i) c(m[i, 1], m[i, 2])))
}

query_polygon_process <- function(polygon_sf, token, collection_id, band_name,
                                  time_from, time_to) {

  evalscript <- sprintf('//VERSION=3
function setup() {
  return {
    input: ["%s", "dataMask"],
    output: { bands: 1, sampleType: "FLOAT32" }
  };
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
        type       = collection_id,
        dataFilter = list(timeRange = list(from = time_from, to = time_to))
      ))
    ),
    output = list(
      width  = 64, height = 64,
      responses = list(list(identifier = "default",
                            format = list(type = "image/tiff")))
    ),
    evalscript = evalscript
  )

  response <- POST(
    "https://services.sentinel-hub.com/api/v1/process",
    add_headers(Authorization  = paste("Bearer", token),
                Accept         = "image/tiff",
                `Content-Type` = "application/json"),
    body = body, encode = "json"
  )

  if (status_code(response) == 200) {
    tmp <- tempfile(fileext = ".tif")
    writeBin(content(response, "raw"), tmp)
    r    <- rast(tmp)
    vals <- values(r)
    vals <- vals[!is.nan(vals)]   # keep all real values, drop only masked pixels
    unlink(tmp)
    if (length(vals) > 0) return(mean(vals, na.rm = TRUE))
  } else {
    message("Error: ", content(response, "text"))
  }
  NA_real_
}

# -------------------------- 6. LOOP OVER SITES × COLLECTIONS -----------------

results <- do.call(rbind, lapply(seq_len(nrow(aoi)), function(i) {
  message(sprintf("Processing %s (%d/%d)", aoi$id[i], i, nrow(aoi)))

  cent <- st_coordinates(st_centroid(aoi[i, ]))
  row  <- data.frame(id  = aoi$id[i],
                     lon = cent[1, 1],
                     lat = cent[1, 2])

  for (m in names(collections)) {
    col <- collections[[m]]
    row[[paste0(m, "_mean")]] <- query_polygon_process(
      polygon_sf    = aoi[i, ],
      token         = bearer_token,
      collection_id = col$id,
      band_name     = col$band,
      time_from     = time_from,
      time_to       = time_to
    )
    Sys.sleep(0.2)
  }
  row
}))

print(results)
write.csv(results, "data/example_points_results.csv", row.names = FALSE)

# -------------------------- 7. SANITY-CHECK MAP ------------------------------

results$popup <- sprintf(
  "<b>%s</b><br>Biomass: %.1f Mg/ha<br>Canopy cover: %.1f %%<br>Canopy height: %.1f m",
  results$id, results$biomass_mean, results$canopy_cover_mean, results$canopy_height_mean
)

leaflet() |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
  addProviderTiles(providers$OpenStreetMap,     group = "OpenStreetMap") |>
  addPolygons(data = aoi, color = "#FF6600", weight = 2,
              fillColor = "#FF6600", fillOpacity = 0.3,
              popup = results$popup, group = "Buffers") |>
  addCircleMarkers(data = results, lng = ~lon, lat = ~lat,
                   radius = 5, color = "#FF0000", fillOpacity = 0.8,
                   popup = ~popup, group = "Points") |>
  addLayersControl(baseGroups = c("Satellite", "OpenStreetMap"),
                   overlayGroups = c("Buffers", "Points"),
                   options = layersControlOptions(collapsed = FALSE))
