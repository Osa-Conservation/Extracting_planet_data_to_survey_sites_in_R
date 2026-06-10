# =============================================================================
# 02_polygon_workflow.R
#
# Workflow for POLYGON data (e.g. restoration plots, conservation easements):
#   1. Read a polygon shapefile.
#   2. Loop over plots × years × metrics, computing the zonal mean each time.
#   3. Plot per-plot and mean (± SE) time series.
#   4. Build a leaflet map coloured by change in biomass (Δ ACD) between two
#      years.
#
# Assumes ~/.Renviron has SH_CLIENT_ID and SH_CLIENT_SECRET set.
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
id_column      <- "plot_id"          # <- the column that uniquely identifies plots

# Years to query (one calendar year per row).
years <- 2014:2024

# Collections to query.
collections <- list(
  biomass       = list(id = "byoc-96357063-a972-4410-a7a9-5e56105ae393", band = "ACD"),
  canopy_cover  = list(id = "byoc-553728e5-7931-4316-8f31-424d53cce475", band = "CC"),
  canopy_height = list(id = "byoc-6a7b70e8-f001-4407-88ed-272069c09dab", band = "CH")
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

# If the shapefile has MULTIPOLYGON features, cast to POLYGON so each row is a
# single ring the API can ingest.
if (any(st_geometry_type(plots_sf) == "MULTIPOLYGON")) {
  plots_sf <- st_cast(plots_sf, "POLYGON")
}

# -------------------------- 4. HELPERS ---------------------------------------

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
      width = 64, height = 64,
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
    vals <- vals[!is.nan(vals)]   # keep zeros — only drop masked pixels
    unlink(tmp)
    if (length(vals) > 0) return(mean(vals, na.rm = TRUE))
  } else {
    message("Error: ", content(response, "text"))
  }
  NA_real_
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
        col <- collections[[m]]
        row[[m]] <- query_polygon_process(
          polygon_sf    = plots_sf[i, ],
          token         = token,
          collection_id = col$id,
          band_name     = col$band,
          time_from     = time_periods$from[t],
          time_to       = time_periods$to[t]
        )
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

ts_long <- ts |>
  pivot_longer(c(biomass, canopy_cover, canopy_height),
               names_to = "metric", values_to = "value")

metric_labels <- c(biomass       = "Biomass (Mg/ha)",
                   canopy_cover  = "Canopy cover (%)",
                   canopy_height = "Canopy height (m)")

ggplot(ts_long, aes(x = year, y = value, colour = plot_id, group = plot_id)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  facet_wrap(~metric, scales = "free_y", ncol = 1,
             labeller = labeller(metric = metric_labels)) +
  labs(title = "Forest structure over time, by plot",
       x = "Year", y = NULL, colour = "Plot") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"),
        legend.position = "bottom")


# -------------------------- 9. MAP — Δ biomass between two years -------------

year_a <- 2017
year_b <- 2024

delta <- ts |>
  filter(year %in% c(year_a, year_b)) |>
  select(plot_id, year, biomass) |>
  pivot_wider(names_from = year, values_from = biomass,
              names_prefix = "b") |>
  mutate(delta      = .data[[paste0("b", year_b)]] - .data[[paste0("b", year_a)]],
         pct_change = 100 * delta / .data[[paste0("b", year_a)]])

plots_map <- plots_sf |> left_join(delta, by = c("id" = "plot_id"))

pal <- colorBin(palette = "RdYlGn",
                domain  = plots_map$delta,
                bins    = c(-Inf, -10, -5, 0, 5, 10, 15, 20, Inf),
                na.color = "grey50")

plots_map$popup <- sprintf(
  "<b>%s</b><br>Biomass %d: %.1f Mg/ha<br>Biomass %d: %.1f Mg/ha<br>Δ: %+.1f Mg/ha (%+.1f %%)",
  plots_map$id,
  year_a, plots_map[[paste0("b", year_a)]],
  year_b, plots_map[[paste0("b", year_b)]],
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
            title = sprintf("Δ Biomass %d→%d<br>(Mg/ha)", year_a, year_b),
            opacity = 0.9) |>
  addLayersControl(baseGroups = c("Satellite", "OpenStreetMap"),
                   options = layersControlOptions(collapsed = FALSE))
