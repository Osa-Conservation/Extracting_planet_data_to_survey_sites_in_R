# Accessing Planet data in R — a 15-minute primer

A short, hands-on introduction to pulling **Planet Forest Carbon Diligence** data into R from points and polygons, using the Sentinel Hub API.

**📖 Tutorial site:** <https://YOUR-GH-USERNAME.github.io/osa-planet-r-tutorial/>

This repository accompanies a 15-minute session for the Osa Conservation science team. The tutorial walks through two workflows:

1. **Points** — given a CSV of lon/lat sites, buffer each into a circular area of interest and extract zonal means.
2. **Polygons** — given a shapefile of plots, extract zonal means directly, optionally as a multi-year time series.

## Quick start

```bash
git clone https://github.com/YOUR-GH-USERNAME/osa-planet-r-tutorial.git
cd osa-planet-r-tutorial
```

Then open the project in RStudio and follow [the tutorial](https://YOUR-GH-USERNAME.github.io/osa-planet-r-tutorial/).

## Repo layout

```
.
├── index.md              # the GitHub Pages tutorial (this is the talk)
├── R/
│   ├── 00_setup.R              # checks packages and tests authentication
│   ├── 01_points_workflow.R    # CSV of points → buffers → zonal means
│   └── 02_polygon_workflow.R   # shapefile → zonal means → time series
├── data/
│   ├── example_points.csv      # 8 example sites in the Osa Peninsula
│   └── README.md               # how to add your own shapefile
└── figures/                    # pre-rendered plots used in the tutorial
```

## Requirements

- R ≥ 4.2
- Packages: `httr`, `sf`, `terra`, `dplyr`, `tidyr`, `ggplot2`, `leaflet`
- A free [Sentinel Hub](https://www.sentinel-hub.com/) account with an OAuth client
- (Optional) Access to a Planet image collection delivered via the Orders/Subscriptions API — sandbox collections work for testing

## Credentials — read this first

Never commit your `client_id` / `client_secret`. Put them in `~/.Renviron`:

```
SH_CLIENT_ID=your-client-id-here
SH_CLIENT_SECRET=your-client-secret-here
```

The scripts in `R/` read them with `Sys.getenv()`. The `.gitignore` already excludes `.Renviron` to make sure they don't sneak into a commit.

## License

Code: MIT. Tutorial text: CC-BY 4.0.
