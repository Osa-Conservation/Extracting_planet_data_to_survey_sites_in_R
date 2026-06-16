# =======================================
# 01_points_workflow.R
#
# Workflow for POINT data:
#   1. Read a CSV of site coordinates.
#   2. Buffer each point into a circular AOI (in metres).
#   3. Query Planet Forest Carbon Diligence layers via the Sentinel Hub
#      Process API and compute the mean over each AOI.
#   4. Save to CSV and plot on a leaflet map.
#
# NOTE!!! This assumes ~/.Renviron has SH_CLIENT_ID and SH_CLIENT_SECRET set. See the setup doc!
# ====================================

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

# Each entry can request several bands at once — the mean of the value band
# plus, if your collection exposes them, the lower (5th %) and upper (95th %)
# prediction-bound bands that make up Planet's 90 % uncertainty interval.
#
#  Band names may vary by collection, so check what ships with the product in the collections Menu
#  IF THERE IS NO UNCERTAINTY - e.g Land surface temperature - Set `lower` / `upper` to NULL 
   
collections <- list(
  biomass = list(
    id    = "byoc-96357063-a972-4410-a7a9-5e56105ae393",
    mean  = "ACD",
    lower = "UC_Q05",
    upper = "UC_Q95"
  ),
  canopy_cover = list(
    id    = "byoc-553728e5-7931-4316-8f31-424d53cce475",
    mean  = "CC",
    lower = "UC_Q05",
    upper = "UC_Q95"
  ),
  canopy_height = list(
    id    = "byoc-6a7b70e8-f001-4407-88ed-272069c09dab",
    mean  = "CH",
    lower = "UC_Q05",
    upper = "UC_Q95"
  )
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

# Check where your locations are projecting
leaflet() |>   addProviderTiles(providers$Esri.WorldImagery) |>   addPolygons(data = aoi, color = "#FF6600", fillOpacity = 0.3,
                                                                              label = aoi$id,
                                                                              labelOptions = labelOptions(
                                                                                noHide = TRUE,          # Makes the label permanently visible
                                                                                direction = "center",   # Centers text on the polygon centroid
                                                                                textOnly = TRUE,        # Removes the default white background box
                                                                                style = list(
                                                                                  "color" = "black",
                                                                                  "font-weight" = "bold",
                                                                                  "font-size" = "12px"
                                                                                ))
                                                                              ) 


# -------------------------- 5. HELPERS ---------------------------------------

sf_to_geojson_coords <- function(polygon_sf) {
  m <- st_coordinates(polygon_sf)[, 1:2]
  list(lapply(seq_len(nrow(m)), function(i) c(m[i, 1], m[i, 2])))
}

query_polygon_process <- function(polygon_sf, token, collection_id, bands,
                                  time_from, time_to) {
  
  # `bands` is a named character vector, e.g.
  #   c(mean = "ACD", lower = "ACD_LB", upper = "ACD_UB")
  # NULL entries (no uncertainty bands) are dropped silently.
  bands  <- bands[!vapply(bands, is.null, logical(1))]
  bands  <- bands[nzchar(bands)]
  n      <- length(bands)
  if (n == 0) stop("No bands requested.")
  
  # Build the JS arrays for the evalscript dynamically.
  inputs  <- paste0('"', c(unname(bands), "dataMask"), '"', collapse = ", ")
  returns <- paste0("sample.", unname(bands), collapse = ", ")
  nans    <- paste0(rep("NaN", n), collapse = ", ")
  
  evalscript <- sprintf('//VERSION=3
function setup() {
  return {
    input: [%s],
    output: { bands: %d, sampleType: "FLOAT32" }
  };
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
  
  # Default return — one NA per requested band, named so the caller can still
  # bind columns even when the query failed.
  empty <- setNames(rep(NA_real_, n), names(bands))
  
  if (status_code(response) != 200) {
    message("Error: ", content(response, "text"))
    return(empty)
  }
  
  tmp <- tempfile(fileext = ".tif")
  writeBin(content(response, "raw"), tmp)
  r    <- rast(tmp)
  
  # Hard-crop to the actual polygon. Sentinel Hub returns a 64×64 grid covering
  # the polygon's bounding box, so the corners of that grid aren't inside the
  # buffer (or shapefile polygon). mask() sets out-of-polygon pixels to NA so
  # they're excluded from colMeans below. CRSs are already aligned — we read
  # the TIFF in EPSG:4326 and the polygon_sf is also in EPSG:4326.
  r <- mask(r, vect(polygon_sf))
  
  # `vals` is a pixels-by-bands matrix. The evalscript masks all bands together
  # via dataMask, so a row is either all-valid or all-NaN.
  vals <- values(r)
  unlink(tmp)
  
  if (!is.matrix(vals)) vals <- matrix(vals, ncol = 1)
  keep <- !is.nan(vals[, 1])
  if (!any(keep)) return(empty)
  
  # Take the mean values
  means <- colMeans(vals[keep, , drop = FALSE], na.rm = TRUE)
  setNames(means, names(bands))
}


# -------------------------- 6. LOOP OVER SITES × COLLECTIONS -----------------

results <- do.call(rbind, lapply(seq_len(nrow(aoi)), function(i) {
  message(sprintf("Processing %s (%d/%d)", aoi$id[i], i, nrow(aoi)))
  
  cent <- st_coordinates(suppressWarnings(st_centroid(aoi[i, ])))
  row  <- data.frame(id  = aoi$id[i],
                     lon = cent[1, 1],
                     lat = cent[1, 2],
                     row.names = i)
  
  for (m in names(collections)) {
    col   <- collections[[m]]
    means <- query_polygon_process(
      polygon_sf    = aoi[i, ],
      token         = bearer_token,
      collection_id = col$id,
      bands         = c(mean  = col$mean,
                        lower = col$lower,
                        upper = col$upper),
      time_from     = time_from,
      time_to       = time_to
    )
    # `means` is a named vector — e.g. mean, lower, upper. Fan out to columns
    # like biomass_mean, biomass_lower, biomass_upper.
    for (nm in names(means)) {
      row[[paste0(m, "_", nm)]] <- unname(means[nm])
    }
    Sys.sleep(0.2)
  }
  row
}))

print(results)
write.csv(results, "data/example_points_results.csv", row.names = FALSE)

# -------------------------- 7. SANITY-CHECK MAP ------------------------------

# Make a vector with the resuls to pop up (ordered the same as the dataframe)
tmp_popup <- sprintf(
  "<b>%s</b><br>Biomass: %.1f Mg/ha [%.1f – %.1f]<br>Canopy cover: %.1f %% [%.1f – %.1f]<br>Canopy height: %.1f m [%.1f – %.1f]",
  results$id,
  results$biomass_mean,       results$biomass_lower,       results$biomass_upper,
  results$canopy_cover_mean,  results$canopy_cover_lower,  results$canopy_cover_upper,
  results$canopy_height_mean, results$canopy_height_lower, results$canopy_height_upper
)

leaflet() |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
  addProviderTiles(providers$OpenStreetMap,     group = "OpenStreetMap") |>
  addPolygons(data = aoi, color = "#FF6600", weight = 2,
              fillColor = "#FF6600", fillOpacity = 0.3,
              popup = tmp_popup, group = "Buffers") |>
  addCircleMarkers(data = results, lng = ~lon, lat = ~lat,
                   radius = 5, color = "#FF0000", fillOpacity = 0.8,
                   popup = tmp_popup, group = "Points") |>
  addLayersControl(baseGroups = c("Satellite", "OpenStreetMap"),
                   overlayGroups = c("Buffers", "Points"),
                   options = layersControlOptions(collapsed = FALSE))
