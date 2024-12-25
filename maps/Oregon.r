# Inspired by 
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

# 2. Oregon BORDER
#-------------------

path <- here::here('maps/data')

# This includes quite a bit of water on the coast
states = tigris::states()
state_crs = st_crs(2992) # Oregon Lambert
oregon = states |> filter(NAME=='Oregon')

# This is a bit low res but OK...
oregon = usmap::us_map(include='Oregon') |> 
  st_transform(state_crs)

# 3. RIVERS and LAKES
#-------------------

water_bodies = st_read(file.path(path,
  'HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na.shp'))
water_bodies = water_bodies |> st_transform(state_crs)
water_bodies= water_bodies[oregon,]

mapview(water_bodies) + mapview(oregon)

# River width
#------------

river_width <- water_bodies |>
    dplyr::mutate(
        width = as.numeric(
            ORD_FLOW
        ),
        width = dplyr::case_when(
            width == 3 ~ 6, 
            width == 4 ~ 5,
            width == 5 ~ 4,
            width == 6 ~ 3,
            width == 7 ~ 2,
            TRUE ~ 1
        )
    )


# 6. DEM
#-------

dem <- elevatr::get_elev_raster(
    locations = oregon,
    z = 8, clip = "locations"
)

dem_state <- dem |>
    terra::rast() |>
    terra::project(oregon)

dem_matrix <- rayshader::raster_to_matrix(
    dem_state
)

# 2. Land Cover RASTER
#----------------------

path = here::here('maps/data/Oregon_land_cover.tif')

land_cover <- terra::rast(path)

vals <- terra::values(
    land_cover,
    dataframe = T
)

names(vals)
names(vals)[1] <- "value"
unique(vals$value)

# 3. CROP Land Cover RASTER
#---------------------------

country_land_cover <- terra::project(land_cover, 'epsg:2992')

country_land_cover_resampled <- terra::resample(
    x = country_land_cover['Oregon_land_cover_1',],
    y = dem_state, method = "near"
)
terra::plot(country_land_cover_resampled)

# 4. Land Cover RASTER TO IMAGE
#-------------------------------

# From "nlcd_2021_land_cover_l48_20230630_WHD7sUg1ZU0HbVS6Agh3.xml"
colors = 
'Value    Red         Green      Blue
0           0           0            0
11         70           107          159
12         209          222          248
21         222          197          197
22         217          146          130
23         235          0               0
24         171          0               0
31         179          172          159
41         104          171          95
42         28            95            44
43         181           197         143
52         204           184         121
71         223           223         194
81         220           217         57
82         171           108         40
90         184           217         235
95         108           159         184' |> 
  str_replace_all(' +', ' ') |> 
  read_delim(delim=' ', skip=1, trim_ws=TRUE)

from = colors[[1]]
to <- as.matrix(colors[2:4])

forest_terra <- na.omit(
    country_land_cover_resampled
)

land_cover_image <- terra::subst(
    as.int(forest_terra),
    from,
    to
)

terra::plotRGB(land_cover_image)
rm(land_cover, vals, country_land_cover, forest_terra)
gc()

#lc_matrix = terra::as.matrix(land_cover_image)

img_file <- "maps/Oregon_land_cover_image.png"
terra::writeRaster(
    land_cover_image,
    img_file,
    overwrite = T,
    NAflag = 255
)

img <- png::readPNG(img_file)

# 7. RENDER SCENE
#----------------

dem_matrix |>
    rayshader::height_shade(
        texture = colorRampPalette('white')(512)
    ) |>
    rayshader::add_overlay(
        img,
        alphalayer = .9,
        alphacolor = "white"
    ) |>
    rayshader::add_shadow(
        rayshader::lamb_shade(
            dem_matrix,
            zscale = 50,
            sunaltitude = 90,
            sunangle = 315,
        ), max_darken = .25
    ) |>
    rayshader::add_shadow(
        rayshader::texture_shade(
            dem_matrix,
            detail = .95,
            brightness = 90, #warn
            contrast = 80,
        ), max_darken = .1
    ) |>
    rayshader::add_overlay(
      rayshader::generate_line_overlay(
        geometry = river_width,
        extent = dem_state,
        heightmap = dem_matrix,
        color = "#387B9C",
        linewidth = river_width$width/2,
        data_column_width = "width"
      )
    ) |> 
    rayshader::plot_3d(
        dem_matrix,
        zscale = 35,
        solid = FALSE,
        shadow = TRUE,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(600, 400),
        zoom = .55,
        phi = 89,
        theta = 0
    )


rayshader::render_camera(
    zoom = .55
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
    filename = here::here("maps/oregon_land_cover_zscale35.png"),
    preview = TRUE,
    light = FALSE,
    environment_light = hdri_file,
    intensity_env = 1,
    interactive = FALSE,
    width = 3000,
    height = 2000
)
