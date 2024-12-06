# Explore watershed data
library(tidyverse)
library(mapview)
library(sf)

# This is MA only
ct_river_watershed = 
  read_sf(here::here('maps/data/megabasins/MEGABASINS_POLY.shp')) |> 
  filter(NAME=='CONNECTICUT')

rivers = read_sf(here::here('maps/data/hydro25k/HYDRO25K_ARC.shp'))
rivers = rivers[ct_river_watershed,]
mapview(rivers, zcol='ARC_CODE')

water_features <- c(
  "0" = "UNKNOWN",
  "1" = "SHORELINE",
  "2" = "CLOSURE LINE",
  "3" = "APPARENT WETLAND LIMIT",
  "4" = "STREAM",
  "5" = "INTERMITTENT STREAM",
  "6" = "DITCH, CANAL",
  "7" = "AQUEDUCT",
  "8" = "DAM",
  "9" = "INTERMITTENT/INDEFINITE SHORELINE",
  "10" = "MAN-MADE SHORELINE",
  "11" = "CHANNEL IN WATER",
  "12" = "UNKNOWN",
  "99" = "TRANSPORT ARC"
)

rivers$type = water_features[as.character(rivers$ARC_CODE)]
# Start filtering
rivers |> 
  filter(!ARC_CODE %in% c(3, 5, 6, 7, 9, 10)) |> 
  mapview(zcol='type', color=RColorBrewer::brewer.pal(6, 'Dark2'))

# Don't care about these
rivers |> 
  filter(ARC_CODE %in% c(0, 2, 8, 12, 99)) |> 
  mapview(zcol='type', color=RColorBrewer::brewer.pal(6, 'Dark2'))

# These seem to be the good ones...
rivers |> 
  filter(ARC_CODE %in% c(1, 4)) |> 
  mapview(zcol='type', color=RColorBrewer::brewer.pal(6, 'Dark2'))

# Now water bodies
water = read_sf(
  here::here('maps/data/hydro25k/HYDRO25K_POLY.shp')
)

water = water[ct_river_watershed,]
water_areas <- c(
 "0" = "LAND/ISLAND/DAM/AQUEDUCT",
 "1" = "RESERVOIR (with PWSID)",
 "2" = "WETLAND, MARSH, SWAMP, BOG", 
 "3" = "SUBMERGED WETLANDS",
 "4" = "CRANBERRY BOG",
 "5" = "SALT WETLANDS",
 "6" = "LAKE, POND, WIDE RIVER, IMPOUNDMENT",
 "7" = "TIDAL FLATS, SHOALS",
 "8" = "BAY, OCEAN",
 "9" = "INUNDATED AREA"
)
water$type = water_areas[as.character(water$POLY_CODE)]
water = st_cast(water, 'POLYGON')
water |> st_drop_geometry() |> count(type)
water |> 
  filter(POLY_CODE %in% c(1, 6, 9)) |> 
  mapview(zcol='type', col.regions=RColorBrewer::brewer.pal(6, 'Dark2'))

# Water code 6 (river) is a filled version of rivers code 1 (shoreline)
# so we only need the water version
mapview(water |> filter(POLY_CODE %in% c(1, 6, 9))) +
  mapview(rivers |> filter(ARC_CODE %in% c(4)))

# OK let's do this...
library(terra)
library(elevatr)
library(rayshader)

dem <- elevatr::get_elev_raster(
    locations = ct_river_watershed,
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
            geometry = rivers |> filter(ARC_CODE %in% c(4)),
            extent = dem_country,
            heightmap = dem_matrix,
            color = "#387B9C",
            linewidth = 2
        ), alphalayer = 1
    ) |>
    rayshader::add_overlay(
      rayshader::generate_polygon_overlay(
        geometry = (water |> filter(POLY_CODE %in% c(1, 6, 9))),
        extent = dem_country,
        heightmap=dem_matrix,
        linecolor="#387B9C",
        palette = "#387B9C",
      ), alphalayer=1
    ) |> 
    rayshader::plot_3d(
        dem_matrix,
        zscale = 8,
        solid = FALSE,
        shadow = TRUE,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(600, 600),
        zoom = .7,
        phi = 89,
        theta = 0
    )

rayshader::render_camera(
    zoom = .7
)

# 8. RENDER OBJECT
#-----------------

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/photo_studio_loft_hall_4k.hdr"
hdri_file <- here::here('maps/data', basename(u))

download.file(
    url = u,
    destfile = hdri_file,
    mode = "wb"
)

rayshader::render_highquality(
    filename = here::here('maps/ct_watershed_zscale8.png'),
    preview = TRUE,
    light = FALSE,
    environment_light = hdri_file,
    intensity_env = 1,
    interactive = FALSE,
    width = 3000,
    height = 3000
)
