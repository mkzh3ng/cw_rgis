#' Final assignment 

# Looking into presence/absence of nocomis leptocephalus with 
# precipitation data

rm(list = ls())

if(!require(pacman)) install.packages(pacman)

pacman::p_load(tidyverse,
               ggeffects,
               sf,
               terra,
               tidyterra,
               exactextractr,
               mapview)


# format fish data --------------------------------------------------------
df_finsync <- read_csv("data/data_finsync_nc.csv")

df_chub <- df_finsync %>% 
  mutate(presence = 1) %>% 
  pivot_wider(id_cols = c(site_id, lon, lat),
              names_from = latin,
              values_from = presence,
              values_fill = 0) %>% 
  select(site_id,
         lon,
         lat,
         "Nocomis leptocephalus") %>% 
  rename(y = "Nocomis leptocephalus")

sf_chub <- df_chub %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326)

# visualization -----------------------------------------------------
spr_prec_nc <- rast("data/spr_prec_nc.tif")

ggplot() + 
  geom_spatraster(data = spr_prec_nc) + 
  geom_sf(
    data = sf_chub,
    aes(color = factor(y))
  ) + 
  scale_fill_viridis_c() + 
  theme_bw()

# GLM analysis ------------------------------------------------------------
sf_chub_prec <- extract(x = spr_prec_nc,
                      y = sf_chub,
                      bind = TRUE) %>% 
  st_as_sf()

df_chub_prec <- sf_chub_prec %>% 
  as_tibble() %>% 
  select(-geometry)

m_chub <- glm(y ~ precipitation,
              data = df_chub_prec,
              family = "binomial")

summary(m_chub)

# based on the P values, precipitation has a significant effect on presence  
# or absence of bluehead chub

# ggpredict graph ---------------------------------------------------------
df_pred <- ggpredict(m_chub, terms = "precipitation")

plot(df_pred) +
  labs(
    title = "Bluehead chub Species Distribution vs Precipitation levels",
    x = "precipitation",
    y = "probability of species presence"
  ) + 
  theme_minimal()


# Part 2: human footprint index vs species richness -----------------------
view(df_finsync)

df_rich <- df_finsync %>%
  group_by(site_id) %>%
  summarise(species_richness = n_distinct(latin))

df_rich <- df_finsync %>% 
  left_join(df_rich)

# Human footprint layer ---------------------------------------------------
spr_hfp <- rast("data/hfp2018.tif")
sf_nc_county <- readRDS("data/sf_nc_county.rds")

sf_nc_county_prj <- st_transform(sf_nc_county, crs = crs(spr_hfp))
spr_hfp <- crop(spr_hfp, sf_nc_county_prj)

# transform df to sf and change crs
sf_rich <- df_rich %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs= 4326)
          
sf_rich_prj <- st_transform(sf_rich, crs(spr_hfp))

ggplot() + 
  geom_sf(data = sf_rich,
          aes(color = species_richness))

sf_extract <- extract(
  x = spr_hfp,
  y = sf_rich_prj,
  bind = TRUE
) %>% 
  st_as_sf()

mapview(sf_rich_prj, zcol = "species_richness") +
  mapview(spr_hfp)

sf_maybe <- sf_extract %>% 
  mutate(sp_hfp = species_richness/hfp2018)

mapview(sf_maybe, zcol = "sp_hfp")

m_rich <- glm(species_richness ~ hfp2018,
    data = sf_extract,
    family = "poisson")

summary(m_rich)


