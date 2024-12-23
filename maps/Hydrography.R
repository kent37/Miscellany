# Explore sources for hydrography data
# boundary_all, rivers, ponds are from Bike_map.R

hyrdorivers = read_sf(here::here('maps/data/HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na.shp')) |> 
  st_transform(26986)
hyrdorivers = hyrdorivers[boundary_all,]

mapview(hyrdorivers) + mapview(rivers, color='red')

# Try OSM rivers
library(osmdata)

available_features()

# Get a bounding box in lat-long
bbox = st_bbox(boundary_all) |> 
  st_as_sfc() |> 
  st_transform(4326) |> 
  st_bbox()

osm_water = opq(bbox) %>% 
  add_osm_feature("water") %>% 
  osmdata_sf() 

osm_waterway = opq(bbox) %>% 
  add_osm_feature("waterway") %>% 
  osmdata_sf() 

mapview(osm_water$osm_lines) + 
  mapview(osm_water$osm_polygons |> 
            filter(is.na(place) | 
                     (!is.na(place) & !place %in% c('island', 'islet'))) |> 
            filter(is.na(natural) | 
                     (!is.na(natural) & !natural %in% c('wood')))) +
  mapview(osm_water$osm_multipolygons) +
  mapview(osm_waterway$osm_lines) +
  mapview(rivers, color='red') +
  mapview(ponds, color='red', col.regions='red')

mapview(osm_waterway$osm_lines |> filter(waterway=='river')) +
  mapview(rivers, color='red') +
  mapview(ponds, color='red', col.regions='red') +
  mapview(boundary_all |> st_as_sf(), color='black', alpha.regions=0)

# Overture maps
library(DBI)
library(duckdb)

# Create a connection to DuckDB
con <- dbConnect(duckdb())

# Install the spatial  & htttpfs extensions
dbExecute(con, "INSTALL spatial;")
dbExecute(con, "LOAD spatial;")
dbExecute(con, "INSTALL httpfs;")
dbExecute(con, "LOAD httpfs;")
dbExecute(con, "SET s3_region='us-west-2';")

# Get WKT for the query
st_as_text(bbox |> st_as_sfc())

# This seems to be doing the intersection locally,
# i.e. downloading the entire data set to filter it.
query <- "
SELECT *, ST_AsText(geometry) as geometry_wkt
FROM read_parquet('s3://overturemaps-us-west-2/release/2024-11-13.0/theme=base/type=water/*')
WHERE ST_Intersects(
  ST_MakeEnvelope(    
    bbox.xmin,  -- west
    bbox.ymin,  -- south
    bbox.xmax,  -- east
    bbox.ymax  -- north
),
  ST_GeomFromText('POLYGON ((-72.946 41.77682, -72.2025 41.77682, -72.2025 42.86986, -72.946 42.86986, -72.946 41.77682))')
)
LIMIT 10;
"

# Execute the query
results <- dbGetQuery(con, query)

# Try the example from the website
dbExecute(con, "INSTALL json;")
dbExecute(con, "LOAD json;")

query = "
SELECT
       id,
       names.primary as name,
       confidence AS confidence,
       CAST(socials AS JSON) as socials,    -- Ensure each attribute can be serialized to JSON
       geometry                             -- DuckDB understands this to be a geometry type
    FROM read_parquet('s3://overturemaps-us-west-2/release/2024-11-13.0/theme=places/type=place/*', filename=true, hive_partitioning=1)
    WHERE categories.primary = 'pizza_restaurant'
    AND bbox.xmin BETWEEN -75 AND -73       -- Only use the bbox min values
    AND bbox.ymin BETWEEN 40 AND 41;"

results <- dbGetQuery(con, query)

# Example from Claude.ai
# For example, to query places in San Francisco:
query <- "
SELECT *, ST_AsText(geometry) as geometry_wkt
FROM read_parquet('s3://overturemaps-us-west-2/release/2024-11-13.0/theme=places/type=place/*')
WHERE ST_Within(
  ST_Point(bbox.xmin, bbox.ymin),
  ST_GeomFromText('POLYGON((-122.5 37.7, -122.5 37.8, -122.4 37.8, -122.4 37.7, -122.5 37.7))')
)
LIMIT 10;
"

# Execute the query
results <- dbGetQuery(con, query)
sf_object <- st_as_sf(results, wkt = "geometry_wkt", crs = 4326)
mapview(st_geometry(sf_object))
dbDisconnect(con, shutdown = TRUE)

# Direct download from duckdb client
"
COPY (
    SELECT names.primary as name, class, subtype, geometry
    FROM read_parquet('s3://overturemaps-us-west-2/release/2024-11-13.0/theme=base/type=water/*')
    WHERE
        bbox.xmin >= -73.19659
        AND bbox.ymin >= 41.99764
        AND bbox.xmax <= -71.88716
        AND bbox.ymax <= 42.74154
    LIMIT 10
) 
TO 'ct_watershed_rivers.gpkg' WITH (FORMAT GDAL, DRIVER 'GPKG');
"

# Try a downloaded version
# Can't read :-(
overture_water = st_read(here::here('maps/data/Massachusetts_hydrography/ct_watershed_rivers.gpkg'))
