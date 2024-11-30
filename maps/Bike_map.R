library(tidyverse)
library(mapview)
library(sf)

# Downloaded from Ride with GPS via Profile / Backup
gps_path = here::here('RWGPS/rwgps_backup/')
gps_files = list.files(gps_path, '*.gpx', full.names=TRUE)

tracks = map(gps_files, ~read_sf(.x, layer='tracks'))
tracks = bind_rows(tracks)

# Write as a gpkg so we can subset in QGIS
write_sf(tracks, file.path(gps_path, 'tracks.gpkg'), layer='all_tracks')

tracks = read_sf(file.path(gps_path, 'tracks.gpkg'), layer='selected_tracks')

tracks = tracks |> st_transform(26986) # MA State Plane (m)
tracks = st_geometry(tracks)

# Make limits for mapping
boundary = st_convex_hull(tracks) |> 
  st_buffer(10000)
boundary_all = st_union(boundary)

# https://www.mass.gov/info-details/massgis-data-major-ponds-and-major-streams
rivers = read_sf(here::here('maps/data/majorhydro/MAJSTRM_ARC.shp'))
rivers = rivers[boundary_all,]

ponds = read_sf(here::here('maps/data/majorhydro/MAJPOND_POLY.shp'))
ponds = ponds[boundary_all,]

# OK let's do this...
library(terra)
library(elevatr)
library(rayshader)

# Elevation data
dem <- elevatr::get_elev_raster(
    locations = st_as_sf(boundary_all),
    z = 10, clip = "locations"
)

dem_country <- dem |>
    terra::rast() |>
    terra::project('epsg:26986')

dem_matrix <- rayshader::raster_to_matrix(
    dem_country
)

dem_matrix |>
    rayshader::height_shade(
        texture = colorRampPalette(
            c(
                "#fcc69f",
                "#c67847"
            )
        )(128)
    ) |>
    rayshader::add_overlay(
        rayshader::generate_line_overlay(
            geometry = rivers,
            extent = dem_country,
            heightmap = dem_matrix,
            color = "#387B9C",
            linewidth = 2
        ), alphalayer = 1
    ) |>
    rayshader::add_overlay(
      rayshader::generate_polygon_overlay(
        geometry = ponds,
        extent = dem_country,
        heightmap=dem_matrix,
        linecolor="#387B9C",
        palette = "#387B9C",
      ), alphalayer=1
    ) |> 
    rayshader::add_overlay(
        rayshader::generate_line_overlay(
            geometry = tracks,
            extent = dem_country,
            heightmap = dem_matrix,
            color = "#9E2B25",
            linewidth = 3
        ), alphalayer = 0.9
    ) |>
    rayshader::plot_3d(
        dem_matrix,
        zscale = 8,
        solid = FALSE,
        shadow = TRUE,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(400, 600),
        zoom = .6,
        phi = 89,
        theta = 0
    )

rayshader::render_camera(
    zoom = 0.6
)

# 8. RENDER OBJECT
#-----------------

# Downloaded from here...
u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/photo_studio_loft_hall_4k.hdr"
hdri_file <- here::here('maps/data', basename(u))

rayshader::render_highquality(
    filename = here::here('maps/rides.png'),
    preview = TRUE,
    light = FALSE,
    environment_light = hdri_file,
    intensity_env = 1,
    interactive = FALSE,
    width = 2000,
    height = 3000
)
