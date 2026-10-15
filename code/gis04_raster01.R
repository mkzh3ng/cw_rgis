if(!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,
               terra,
               tidyterra,
               mapview,
               stars)


# raster data format ------------------------------------------------------
# rast() is a function to read data from your directory
spr_ex <- rast("data/spr_example.tif")

# writeRaster() is a function to export raster data
writeRaster(x = spr_ex,
            filename = "data/spr_elev.tif",
            overwrite = TRUE)

# visualize raster data; geom_spatraster() (from "tidyterra" package)
ggplot()+
  geom_spatraster(
    data = spr_ex
  )

# if you want mapview() function to work
star_ex <- st_as_stars(spr_ex)
mapview(star_ex)

#  data type in raster ----------------------------------------------------
# continuous
v_elev <- values(spr_ex)

extract(spr_ex, y = cbind(6.0000, 50.0000)) # long. is always first
extract(spr_ex, y = cbind(6.0750, 50.0800))
extract(spr_ex, y = cbind(6.3600, 49.5000))
extract(spr_ex, y = cbind(6.1000, 49.8500))

#extract data at multiple points
df_point <- tibble(
  lon = c(6.1, 6),
  lat = c(50, 50.05)
)

extract(spr_ex, y = df_point)

# discrete data
spr_for <- rast("data/spr_forest_nc.tif")

ggplot()+
  geom_spatraster(data = spr_for)

unique(spr_for)

v_binary <- values(spr_for)
mean(v_binary)*100

#multiple classes - code values with multiple categories
spr_land <- rast("data/spr_land_reclass.tif")

# 1001 = forest
# 1010 = crop
# 1100 = urban
unique(spr_land)

# coordinate, lon -79.8063, lat 36.0701
extract(spr_land, cbind(-79.8063, 36.0701))

# reclass
cm <- cbind(
  c(0, 1001, 1010, 1100),
  c(0, 1, 0, 0)
)

spr_bin <- classify(spr_land,
                    rcl = cm)

unique(spr_bin)

v_bin <- values(spr_bin)
mean(v_bin)*100

# calculate % cropland and % urban
cm_1 <- cbind(
  c(0, 1001, 1010, 1100),
  c(0, 0, 1, 0)
)

spr_bin1 <- classify(spr_land,
                    rcl = cm_1)

v_bin1 <- values(spr_bin1)
mean(v_bin1)*100 # 9.0615% cropland

cm_2 <- cbind(
  c(0, 1001, 1010, 1100),
  c(0, 0, 0, 1)
)

spr_bin2 <- classify(spr_land,
                     rcl = cm_2)

v_bin2 <- values(spr_bin2)
mean(v_bin2)*100 # 3.1695% urban 

# exercise ----------------------------------------------------------------
## 1
spr_prec_ncne <- terra::rast("data/spr_prec_ncne.tif")

## 2
# 162 rows, 532 columns
# resolution is 0.0083,0.0083 (x, y)
# Spatial extent (x = -79.89, -75.46) (y = 35.24, 36.59)
# CRS = 4326
# Minimum precipitation = 1063.1, Maximum precipitation = 1501.5

## 3
ggplot()+
  geom_spatraster(data = spr_prec_ncne)

## 4
sf_site <- readRDS("data/sf_finsync_nc.rds")

df_xy <- st_coordinates(sf_site)

df_lanp <- extract(spr_land, y = df_xy)
unique(spr_land)

table(df_lanp$code)
# 1001 (forest) is the most common land use type

df_lanp %>% 
  filter(code == 1001) %>% 
  nrow()
# 111 forest

df_lanp %>% 
  filter(code == 1010) %>% 
  nrow()
# 2 cropland

df_lanp %>% 
  filter(code == 1100) %>% 
  nrow()
# 9 urban
