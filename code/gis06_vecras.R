#' raster/vector interaction 

rm(list = ls())
if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               sf,
               terra,
               tidyterra,
               exactextractr)

## data
## finsync survey site
sf_site <- readRDS("data/sf_finsync_nc.rds")

## county polygons
sf_nc_county <- readRDS("data/sf_nc_county.rds")

## precipitation raster
spr_prec_nc <- rast("data/spr_prec_nc.tif")

## point-wise extraction 
ggplot() + 
  geom_spatraster(data = spr_prec_nc) + 
  geom_sf(data = sf_site) + 
  scale_fill_viridis_c()

extract(
  x = spr_prec_nc,
  y = sf_site,
  bind = TRUE
) %>% 
  st_as_sf()

## Zonal statistics

sf_nc_county_proj<- st_transform(
  sf_nc_county,
  crs = 32617 # epsg code for UTM zone 17N
)

spr_prec_nc_proj <- project(
  x = spr_prec_nc,
  y = crs(sf_nc_county_proj)
) # this is a raster layer so need project function 

ggplot() + 
  geom_spatraster(data = spr_prec_nc) + 
  geom_sf(data = sf_nc_county, alpha = 0.25)

df_prec_county <- exact_extract(x = spr_prec_nc_proj, 
                                y = sf_nc_county_proj, 
                                fun = "mean", 
                                append_cols = "county") %>% 
  as_tibble() %>% 
  rename(precipitation = mean)

sf_nc_county_prec <- sf_nc_county %>% 
  left_join(df_prec_county,
            by = "county")

## visualize the map with precipitation 
## aes(fill = precipitation)

ggplot() + 
  geom_sf(data = sf_nc_county_prec,
          aes(fill = precipitation)) + 
  scale_fill_viridis_c()

## which county has the lowest precipitation 
sf_nc_county_prec %>% 
  arrange(precipitation)
## warren county!

# buffer-based analysis ---------------------------------------------------
sf_site_proj <- sf_site %>% 
  st_transform(crs = 32617)

sf_site_buffer_proj <- sf_site_proj %>% 
  st_buffer(dist = 10000) # create a buffer with km radius

## visualize the sf_site_proj and sf_site_buffer_proj together
ggplot() + 
  geom_sf(data = sf_site_buffer_proj, alpha = 0.25,) + 
  geom_sf(data = sf_site_proj, alpha = 0.5)

## calculate mean precipitation in each polygon 
df_prec_buffer <- exact_extract(x = spr_prec_nc_proj, 
                                y = sf_site_buffer_proj, 
                                fun = "mean", 
                                append_cols = "site_id") %>% 
  as_tibble() %>% 
  rename(precipitation = mean)

## combine df_prec_buffer with sf_site

sf_site_prec_buffer <- sf_site %>% 
  left_join(df_prec_buffer,
            by = "site_id")

## visualize 
ggplot() + 
  geom_sf(data = sf_nc_county, fill = "tomato", alpha = 0.25) + 
  geom_sf(data = sf_site_prec_buffer,
          aes(color = precipitation)) + 
  scale_color_viridis_c()

# exercise ----------------------------------------------------------------

# 1001 = forest 
# 1010 = crop 
# 1100 = urban
spr_land <- rast("data/spr_land_reclass.tif")

# 1) reclassify spr_land to binary or crop (1) or else (0)
cm <- cbind(
  c(0, 1001, 1010, 1100),
  c(0, 0, 1, 0)
)

spr_crop <- classify(spr_land,
                     rcl = cm)

# 2) calculate the proportion of crop lands in each county
# exact_extract(
# x = raster layer
# y = mask layer
# fun = 'mean' (what summary statistic)
# append_cols = column for labels)
spr_crop_proj <- project(x = spr_crop,
                         y = crs(sf_nc_county_proj))

df_crop <- exact_extract(x = spr_crop_proj, 
                         y = sf_nc_county_proj,
                         fun = "mean", 
                         append_cols = "county") %>% 
  as_tibble() %>% 
  rename(crop = mean)

df_crop <- df_crop %>% 
  mutate(crop = crop*100)

# 3) identify the county with the highest crop fraction
df_crop %>% 
  arrange(desc(crop))
## pasquotank seems to have the most crop land proportion. 

# 4) visualize the fraction of crop by county

sf_nc_crop <- sf_nc_county_proj %>% 
  left_join(df_crop,
            by = "county")

ggplot() + 
  geom_sf(data = sf_nc_crop, 
          aes(fill = crop)) + 
  scale_colour_viridis_c()
