# https://github.com/milos-agathon/3d-river-maps/blob/main/R/switzerland.r
# 1. LIBRARIES
#-------------

library(terra)
library(elevatr)
library(sf)
library(geodata)
library(mapview)
library(rayshader)
library(tidyverse)

# 2. Watershed BORDER
#-------------------

path <- here::here('maps/data')

watersheds = st_read(file.path(path, 
  'Massachusetts_hydrography/Mega Basins/GISDATA_MEGABASINS_POLYPolygon.shp'))
ct_watershed = watersheds |> filter(name=='CONNECTICUT')

# 3. RIVERS and LAKES
#-------------------

water_bodies = st_read(file.path(path,
  'Massachusetts_hydrography/Major Ponds/GISDATA_MAJPOND_POLYPolygon.shp'))
water_bodies = water_bodies[ct_watershed,]

state_rivers <- sf::st_read(file.path(path, 
  'Massachusetts_hydrography/Major Streams/GISDATA_MAJSTRM_ARCLine.shp'))
state_rivers = state_rivers[ct_watershed,]

mapview(ct_watershed) + mapview(water_bodies) + mapview(state_rivers)

# 5. RIVER WIDTH
#---------------

# sort(
#     unique(
#         state_rivers$ORD_FLOW
#     )
# )

# crs_state <- "+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs +type=crs"
# 
# state_river_width <- state_rivers |>
#     dplyr::mutate(
#         width = as.numeric(
#             ORD_FLOW
#         ),
#         width = dplyr::case_when(
#             width == 3 ~ 16, 
#             width == 4 ~ 14,
#             width == 5 ~ 12,
#             width == 6 ~ 10,
#             width == 7 ~ 6,
#             TRUE ~ 0
#         )
#     ) |>
#     sf::st_as_sf() |>
#     sf::st_transform(crs = crs_state)

# 6. DEM
#-------

dem <- elevatr::get_elev_raster(
    locations = ct_watershed,
    z = 10, clip = "locations"
)

dem_state <- dem |>
    terra::rast() |>
    terra::project(ct_watershed)

dem_matrix <- rayshader::raster_to_matrix(
    dem_state
)

# 7. RENDER SCENE
#----------------

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
            geometry = state_rivers,
            extent = dem_state,
            heightmap = dem_matrix,
            color = "#387B9C",
            linewidth = 2
        ), alphalayer = 1
    ) |>
    rayshader::add_overlay(
      rayshader::generate_polygon_overlay(
        geometry = water_bodies,
        extent = dem_state,
        heightmap = dem_matrix,
        linecolor = "#387B9C",
        palette = "#387B9C"
      )
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
hdri_file <- file.path(path, basename(u))

download.file(
    url = u,
    destfile = hdri_file,
    mode = "wb"
)

rayshader::render_highquality(
    filename = here::here("maps/ct_river_watershed_zscale9.png"),
    preview = TRUE,
    light = FALSE,
    environment_light = hdri_file,
    intensity_env = 1,
    interactive = FALSE,
    width = 3000,
    height = 3000
)
