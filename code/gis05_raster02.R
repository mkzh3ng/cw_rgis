#' raster 2

if(!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse, 
               terra,
               tidyterra,
               mapview,
               stars)

rm(list = ls())


# precipitation example ---------------------------------------------------

(spr_prec <- rast("data/spr_prec_us.tif"))

ext(spr_prec)

ggplot() + 
  geom_spatraster(data = spr_prec)

spr_prec_crop <- crop(x = spr_prec,
                      y = c(-80, -75, 34, 37))

## a map of cropped data

ggplot() + 
  geom_spatraster(data = spr_prec_crop)

## get county polygon data
sf_nc_county <- readRDS("data/sf_nc_county.rds")

ggplot() + 
  geom_spatraster(data = spr_prec_crop) + 
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

## use polygon layer to crop raster
spr_prec_nc <- crop(x = spr_prec,
                    y = sf_nc_county)

## map spr_prec_nc with sf_nc_county

ggplot() + 
  geom_spatraster(data = spr_prec_nc) + 
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

# merge -------------------------------------------------------------------

spr_nw <- rast("data/spr_prec_ncnw.tif")
spr_ne <- rast("data/spr_prec_ncne.tif")
spr_sw <- rast("data/spr_prec_ncsw.tif")
spr_se <- rast("data/spr_prec_ncse.tif")

## north west portion of NC
ggplot() + 
  geom_spatraster(data = spr_nw) + 
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

## south west portion of NC
ggplot() + 
  geom_spatraster(data = spr_nw) + 
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

## merge() function to combine 2 panels 
spr_n <- merge(spr_nw, spr_ne)

ggplot() + 
  geom_spatraster(data = spr_n) + 
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

## merge more than two panels into one
list_spr <- list(spr_ne, 
                 spr_nw,
                 spr_se,
                 spr_sw)

## convert from list to spatraster collection
spr_col <- sprc(list_spr)

## merge more than 2 panels
spr_merge <- merge(spr_col)

## map spr_merge and sf_nc_county
ggplot() + 
  geom_spatraster(data = spr_merge) + 
  geom_sf(data = sf_nc_county,
          alpha = 0.25,
          fill = "purple4")

writeRaster(spr_merge, 
            "data/spr_prec_nc.tif",
            overwrite = TRUE)

# stack -------------------------------------------------------------------

spr_prec_nc <- rast("data/spr_prec_nc.tif")
spr_temp_nc <- rast("data/spr_tmp_nc.tif")

ggplot() + 
  geom_spatraster(data = spr_temp_nc) + 
  geom_sf(data = sf_nc_county,
          alpha = 0.25)

## create a multi-layer raster("stack")
spr_pt_nc <- c(spr_prec_nc,
               spr_temp_nc)

## access one layer
## - precipitation
spr_pt_nc$precipitation

## - temperature
spr_pt_nc$temperature

## try to stack "spr_ne" and spr_temp_nc"
## c(spr_ne, spr_temp_nc)
## doesn't work because of the spatial extent mismatch

# reprojection ------------------------------------------------------------

# reprojection - transform CRS from one to another 
# most cases, from geodetic to projected CRS
spr_prec_nc_proj <- project(x = spr_prec_nc,
                            y = "EPSG:32617") # 32617 UTM Zone 17N

# Exercise ----------------------------------------------------------------

## 1) merge raster files
tmp_nw <- rast("data/spr_tmp_ncnw.tif")
tmp_ne <- rast("data/spr_tmp_ncne.tif")
tmp_sw <- rast("data/spr_tmp_ncsw.tif")
tmp_se <- rast("data/spr_tmp_ncse.tif")

tmp_list<- list(tmp_ne, 
                tmp_nw,
                tmp_se,
                tmp_sw)

tmp_sprc <- sprc(tmp_list)

tmp_merge <- merge(tmp_sprc)

ggplot() + 
  geom_spatraster(data = tmp_merge) + 
  geom_sf(data = sf_nc_county,
          alpha = 0.25,
          fill = "seagreen3")

## 2) crop raster to a defined extent
sf_camden <- sf_nc_county %>% 
  filter(county == "camden")

ext(sf_camden)

spr_tmp_camden <- crop(x = tmp_merge,
                       y = sf_camden)

ggplot() + 
  geom_spatraster(data = spr_tmp_camden) + 
  geom_sf(data = sf_nc_county,
          alpha = 0.25,
          fill = "hotpink4")

## 3) reproject raser and explore sampling 
spr_tmp_camden_proj <- project(x = spr_tmp_camden,
                               y = "EPSG:32618", 
                               method = "bilinear")

# because this is temperature, it should be "bilinear interpolation"
# discrete data (example: land use [1001,1010,1100], habitat, etc), use near
# for continuous data (temp, rain, etc), use bilinear

print(spr_tmp_camden_proj)

