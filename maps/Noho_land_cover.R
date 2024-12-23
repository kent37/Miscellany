# Land cover in Northampton
library(tidyverse)
library(mapview)
library(sf)

library(terra)
library(elevatr)
library(rayshader)

noho = read_sf(here::here('../Noho/Shapefiles/Noho_outline/Noho_outline.gpkg'))

dem <- elevatr::get_elev_raster(
    locations = noho,
    z = 10, clip = "locations"
)

dem_country <- dem |>
    terra::rast() |>
    terra::project('epsg:26986')

dem_matrix <- rayshader::raster_to_matrix(
    dem_country
)

# 2. Land Cover RASTER
#----------------------

path = here::here('../Noho/data/NLCD_Land_Cover_2001-2021/NLCD_2021_Land_Cover_L48_20230630_WHD7sUg1ZU0HbVS6Agh3.tiff')

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

country_land_cover <- terra::project(land_cover, 'epsg:26986') |> 
  terra::crop(
    terra::vect(noho),
    snap = "in",
    mask = T
)


terra::plot(country_land_cover)

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
    country_land_cover
)

land_cover_image <- terra::subst(
    as.int(forest_terra),
    from,
    to
)

terra::plotRGB(land_cover_image)

img_file <- "maps/land_cover_image.png"
terra::writeRaster(
    land_cover_image,
    img_file,
    overwrite = T,
    NAflag = 255
)

img <- png::readPNG(img_file)

# 6. RENDER SCENE
#----------------

h <- nrow(img)
w <- ncol(img)

dem_matrix |>
    rayshader::height_shade(
        texture = colorRampPalette(
            "white"
        )(512)
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
    rayshader::plot_3d(
        dem_matrix,
        zscale = 5,
        solid = F,
        shadow = T,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(
            w / 5, h / 5
        ),
        zoom = .6,
        phi = 85,
        theta = 0 
    )

# 7. RENDER OBJECT
#-----------------
u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/photo_studio_loft_hall_4k.hdr"
hdri_file <- here::here('maps/data', basename(u))

rayshader::render_highquality(
    filename = "maps/land_cover_noho.png",
    preview = T,
    light = F,
    environment_light = hdri_file,
    intensity_env = 2,
    rotate_env = 90,
    interactive = F,
    parallel = T,
    width = w, height = h
)
