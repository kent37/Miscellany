library(tidyverse)
library(mapview)
library(sf)

# Downloaded from Ride with GPS via Profile / Backup
gps_path = here::here('RWGPS/rwgps_backup/')
gps_files = list.files(gps_path, '*.gpx', full.names=TRUE)

tracks_all = map(gps_files, ~read_sf(.x, layer='tracks'))
tracks_all = bind_rows(tracks_all)

# Write as a gpkg so we can subset in QGIS
write_sf(tracks_all, file.path(gps_path, 'tracks.gpkg'), layer='all_tracks')

# Use QGIS to subset to local tracks
tracks = read_sf(file.path(gps_path, 'tracks.gpkg'), layer='selected_tracks')

tracks = tracks |> st_transform(26986) # MA State Plane (m)
tracks = st_geometry(tracks)

# Make limits for mapping
boundary = st_convex_hull(tracks) |> 
  st_buffer(10000)
boundary_all = st_union(boundary)

# Waterways from MassGIS give good coverage within state
# https://www.mass.gov/info-details/massgis-data-major-ponds-and-major-streams
rivers = read_sf(here::here('maps/data/Massachusetts_hydrography/majorhydro/MAJSTRM_ARC.shp'))
rivers = rivers[boundary_all,]

ponds = read_sf(here::here('maps/data/Massachusetts_hydrography/majorhydro/MAJPOND_POLY.shp'))
ponds = ponds[boundary_all,]

# Waterways from OSM
library(osmdata)

# Get a bounding box in lat-long
bbox = st_bbox(boundary_all) |> 
  st_as_sfc() |> 
  st_transform(4326) |> 
  st_bbox()

# These are too detailed
# osm_water = opq(bbox) %>% 
#   add_osm_feature("water") %>% 
#   osmdata_sf() 

# This gives major waterways outside of Massachusetts
# They overlap well enough with the MassGIS rivers that it is OK
# to use both
osm_waterway = opq(bbox) %>% 
  add_osm_feature("waterway") %>% 
  osmdata_sf() 

# We need a bunch of pieces from this
# osm_lines = osm_water$osm_lines |> 
#   st_geometry() |> 
#   st_transform(26986) |> 
#   (\(x) x[boundary_all,])() |> 
#   st_as_sf()
# 
# osm_polygons = osm_water$osm_polygons |> 
#   filter(is.na(place) | 
#            (!is.na(place) & !place %in% c('island', 'islet'))) |> 
#   filter(is.na(natural) | 
#            (!is.na(natural) & !natural %in% c('wood'))) |> 
#   st_geometry() |> 
#   st_transform(26986) |> 
#   (\(x) x[boundary_all,])() |> 
#   st_as_sf()
# 
# osm_multipolygons = osm_water$osm_multipolygons |> 
#   st_geometry() |> 
#   st_transform(26986) |> 
#   (\(x) x[boundary_all,])() |> 
#   st_as_sf()

osm_waterways = osm_waterway$osm_lines |> 
  filter(waterway=='river') |> 
  st_geometry() |> 
  st_transform(26986) |> 
  (\(x) x[boundary_all,])() |> 
  st_as_sf()

# OK let's make a map...
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
        rayshader::generate_line_overlay(
            geometry = osm_waterways,
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
