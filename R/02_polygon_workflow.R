# =============================================================================
# 02_polygon_workflow.R
#
# Workflow for POLYGON data (e.g. restoration plots, conservation easements):
#   1. Read a polygon shapefile.
#   2. Loop over plots x years x metrics, computing the zonal mean each time —
#      pulling Planet's lower/upper prediction bounds alongside each value.
#   3. Plot per-plot time series with the 90% prediction interval as a ribbon.
#   4. Build a leaflet map coloured by change in biomass (delta ACD) between two
#      years, with the per-year uncertainty interval shown in the popup.
#
# NOTE!!! This assumes ~/.Renviron has SH_CLIENT_ID and SH_CLIENT_SECRET set. See the setup doc!
# =============================================================================

library(httr)
library(sf)
library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)

# -------------------------- 1. CONFIG ----------------------------------------

shapefile_path <- "data/example_shapes.shp"   # <- your shapefile
id_column      <- "id"                        # <- the column that uniquely identifies plots

# Years to query.
# Full - all data all years
#years <- 2014:2024
# Fast - just as an example
years <- c(2017, 2020, 2023)

# Collection IDs to query. Each entry can request several bands at once — the
# value band plus, if your collection exposes them, the lower (5th %) and upper
# (95th %) prediction-bound bands that make up Planet's 90 % uncertainty
# interval.
#
#  Band names may vary by collection, so check what ships with the product in
#  the collections menu. IF THERE IS NO UNCERTAINTY (e.g. land surface
#  temperature) set `lower` / `upper` to NULL.
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

# -------------------------- 3. READ POLYGONS ---------------------------------

plots_sf <- st_read(shapefile_path) |> st_transform(4326)

# Carry the chosen identifier column through as `id` so the loop and map below
# can reference it consistently.
plots_sf$id <- plots_sf[[id_column]]

# If the shapefile has MULTIPOLYGON features, cast to POLYGON so each row is a
# single ring the API can ingest.
if (any(st_geometry_type(plots_sf) == "MULTIPOLYGON")) {
  plots_sf <- st_cast(plots_sf, "POLYGON")
}

# Plot your shapefile to check it all makes sense
leaflet() |>   addProviderTiles(providers$Esri.WorldImagery) |>   addPolygons(data = plots_sf, color = "#FF6600", fillOpacity = 0.3,
                                                                              label = plots_sf$id,
                                                                              labelOptions = labelOptions(
                                                                                noHide = TRUE,          # Makes the label permanently visible
                                                                                direction = "center",   # Centers text on the polygon centroid
                                                                                textOnly = TRUE,        # Removes the default white background box
                                                                                style = list(
                                                                                  "color" = "black",
                                                                                  "font-weight" = "bold",
                                                                                  "font-size" = "12px"
                                                                                )))

# -------------------------- 4. HELPERS ---------------------------------------

sf_to_geojson_coords <- function(polygon_sf) {
  m <- st_coordinates(polygon_sf)[, 1:2]
  list(lapply(seq_len(nrow(m)), function(i) c(m[i, 1], m[i, 2])))
}

query_polygon_process <- function(polygon_sf, token, collection_id, bands,
                                  time_from, time_to) {
  
  # `bands` is a named character vector, e.g.
  #   c(mean = "ACD", lower = "UC_Q05", upper = "UC_Q95")
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
  r <- rast(tmp)
  
  # Hard-crop to the actual polygon. Sentinel Hub returns a 64x64 grid covering
  # the polygon's bounding box, so the corners of that grid aren't inside the
  # plot. mask() sets out-of-polygon pixels to NA so they're excluded from the
  # mean. CRSs are aligned — both the TIFF and polygon_sf are EPSG:4326.
  r <- mask(r, vect(polygon_sf))
  
  # `vals` is a pixels-by-bands matrix. After mask(), out-of-polygon pixels are
  # NA; in-polygon no-data pixels are NaN. is.na() is TRUE for BOTH, so this one
  # check drops everything we don't want and keeps only real values.
  vals <- values(r)
  unlink(tmp)
  
  if (!is.matrix(vals)) vals <- matrix(vals, ncol = 1)
  keep <- !is.na(vals[, 1])
  if (!any(keep)) return(empty)
  
  # Take the mean per band over the surviving (in-polygon, valid) pixels.
  means <- colMeans(vals[keep, , drop = FALSE], na.rm = TRUE)
  setNames(means, names(bands))
}

# -------------------------- 5. BUILD TIME PERIODS ----------------------------

time_periods <- data.frame(
  period = as.character(years),
  from   = sprintf("%d-01-01T00:00:00Z", years),
  to     = sprintf("%d-12-31T23:59:59Z", years),
  year   = years
)

# -------------------------- 6. EXTRACT TIME SERIES ---------------------------

extract_timeseries <- function(plots_sf, token, collections, time_periods) {
  rows <- list()
  for (i in seq_len(nrow(plots_sf))) {
    message(sprintf("Plot %s (%d/%d)", plots_sf$id[i], i, nrow(plots_sf)))
    for (t in seq_len(nrow(time_periods))) {
      message(sprintf("  - %s", time_periods$period[t]))
      row <- data.frame(plot_id = plots_sf$id[i],
                        year    = time_periods$year[t])
      for (m in names(collections)) {
        col   <- collections[[m]]
        means <- query_polygon_process(
          polygon_sf    = plots_sf[i, ],
          token         = token,
          collection_id = col$id,
          bands         = c(mean  = col$mean,
                            lower = col$lower,
                            upper = col$upper),
          time_from     = time_periods$from[t],
          time_to       = time_periods$to[t]
        )
        # Fan out the named vector (mean/lower/upper) into columns like
        # biomass_mean, biomass_lower, biomass_upper.
        for (nm in names(means)) {
          row[[paste0(m, "_", nm)]] <- unname(means[nm])
        }
        Sys.sleep(0.2)
      }
      rows[[length(rows) + 1]] <- row
    }
  }
  do.call(rbind, rows)
}

ts <- extract_timeseries(plots_sf, bearer_token, collections, time_periods)
write.csv(ts, "data/plots_timeseries.csv", row.names = FALSE)

# -------------------------- 7. PLOT — per-plot time series -------------------
# Reshape from wide (biomass_mean, biomass_lower, ...) to long, splitting each
# column name into a metric and a statistic, then widen the statistic back out
# so each row has mean/lower/upper for one plot x year x metric.

ts_long <- ts |>
  pivot_longer(
    cols          = -c(plot_id, year),
    names_to      = c("metric", "stat"),
    names_pattern = "(.*)_(mean|lower|upper)"
  ) |>
  pivot_wider(names_from = stat, values_from = value)

metric_labels <- c(biomass       = "Biomass (Mg/ha)",
                   canopy_cover  = "Canopy cover (%)",
                   canopy_height = "Canopy height (m)")

ggplot(ts_long, aes(x = year, y = mean,
                    colour = plot_id, fill = plot_id, group = plot_id)) +
  # We can also plot the 90% prediction interval as a translucent ribbon. 
  # With many plots these can overlap heavily — so I have commented it out
  # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  facet_wrap(~metric, scales = "free_y", ncol = 1,
             labeller = labeller(metric = metric_labels)) +
  labs(title = "Forest structure over time, by plot",
       x = "Year", y = NULL, colour = "Plot", fill = "Plot") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        legend.position = "bottom")


# -------------------------- 9. MAP — delta biomass between two years ---------

year_a <- 2017
year_b <- 2023

delta <- ts |>
  filter(year %in% c(year_a, year_b)) |>
  select(plot_id, year, biomass_mean, biomass_lower, biomass_upper) |>
  pivot_wider(names_from  = year,
              values_from = c(biomass_mean, biomass_lower, biomass_upper),
              names_sep   = "_") |>
  mutate(delta      = .data[[paste0("biomass_mean_", year_b)]] -
           .data[[paste0("biomass_mean_", year_a)]],
         pct_change = 100 * delta / .data[[paste0("biomass_mean_", year_a)]])

plots_map <- plots_sf |> left_join(delta, by = c("id" = "plot_id"))

pal <- colorBin(palette = "RdYlGn",
                domain  = plots_map$delta,
                bins    = c(-Inf, -10, -5, 0, 5, 10, 15, 20, Inf),
                na.color = "grey50")

# Popup shows each year's mean with its 90% prediction interval, then the
# change. If the two intervals overlap a lot, treat the delta cautiously.
plots_map$popup <- sprintf(
  "<b>%s</b><br>Biomass %d: %.1f [%.1f-%.1f] Mg/ha<br>Biomass %d: %.1f [%.1f-%.1f] Mg/ha<br>Delta: %+.1f Mg/ha (%+.1f %%)",
  plots_map$id,
  year_a, plots_map[[paste0("biomass_mean_",  year_a)]],
  plots_map[[paste0("biomass_lower_", year_a)]],
  plots_map[[paste0("biomass_upper_", year_a)]],
  year_b, plots_map[[paste0("biomass_mean_",  year_b)]],
  plots_map[[paste0("biomass_lower_", year_b)]],
  plots_map[[paste0("biomass_upper_", year_b)]],
  plots_map$delta, plots_map$pct_change
)

leaflet(plots_map) |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
  addProviderTiles(providers$OpenStreetMap,     group = "OpenStreetMap") |>
  addPolygons(fillColor = ~pal(delta), color = "black",
              weight = 1, fillOpacity = 0.7, popup = ~popup,
              highlightOptions = highlightOptions(weight = 3, color = "white",
                                                  fillOpacity = 0.9)) |>
  addLegend(position = "bottomright", pal = pal, values = ~delta,
            title = sprintf("Delta Biomass %d->%d<br>(Mg/ha)", year_a, year_b),
            opacity = 0.9) |>
  addLayersControl(baseGroups = c("Satellite", "OpenStreetMap"),
                   options = layersControlOptions(collapsed = FALSE))
