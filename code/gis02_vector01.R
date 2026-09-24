#' vector 1
rm(list = ls())

if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               sf,
               mapview)

# Read and Export ---------------------------------------------------------

# SHAPE Format
# how to read vector data
sf_nc_county <- st_read(dsn = "data/nc.shp",
        quiet = TRUE)

#how to export shape files
st_write(sf_nc_county,
         dsn = "data/sf_nc_county.shp",
         append = FALSE)

## RDS format
saveRDS(sf_nc_county,
        file = "data/sf_nc_county.rds")

sf_nc_county <- readRDS(file = "data/sf_nc_county.rds")

# Point -------------------------------------------------------------------

# read point vector data
sf_site <- readRDS("data/sf_finsync_nc.rds")

# visualize
mapview(sf_site, 
        col.regions = "black",
        legend = FALSE)

# select first 10 sites
sf_site_f10 <- sf_site %>% 
  slice(1:10)

mapview(sf_site_f10,
        col.regions = "aquamarine",
        legend = FALSE)

# line --------------------------------------------------------------------

sf_str <- readRDS("data/sf_stream_gi.rds")

mapview(sf_str, 
        color = "red",
        legend = FALSE)

# polygon -----------------------------------------------------------------

sf_nc_county <- readRDS("data/sf_nc_county.rds")

mapview(sf_nc_county, 
        col.regions = "steelblue",
        legend = FALSE)

# select guilford county, then map it out 

sf_nc_guilford <- sf_nc_county %>% 
  filter(county == "guilford")

mapview(sf_nc_guilford, 
        col.regions = "pink",
        legend = FALSE)

# static map in ggplot format --------------------------------------------------------------

ggplot() + 
  geom_sf(data = sf_nc_county)

ggplot() +
  geom_sf(data = sf_nc_county) +
  geom_sf(data = sf_str) +
  geom_sf(data = sf_site, color = "red")

# Exercises ---------------------------------------------------------------
## 1: Read stream line data for Ashe county

sf_str_as <- readRDS("data/sf_stream_as.rds")

## 2: Check coordinate reference systems (CDS)

print(sf_str_as)
print(sf_nc_county)
# yes they both have CRS = WGS 84

## 3: Map streams and county boundaries

ggplot() +
  geom_sf(data = sf_nc_county) +
  geom_sf(data = sf_str_as, color = "purple")

## 4:Subset county layer to Ashe county and remap

sf_nc_as <- sf_nc_county %>% 
  filter(county == "ashe")

ggplot() +
  geom_sf(data = sf_nc_as) +
  geom_sf(data = sf_str_as, color = "magenta")
