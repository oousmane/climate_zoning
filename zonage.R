library(tidyverse)
library(gstat)
library(sf)
data <- read_csv2("~/Downloads/normale.csv") %>% 
  as_tibble() %>% 
  rename(
    rain = Precipitation
  ) %>% 
  select(long,lat, rain)
data

std_norm9120_network <- data %>% 
  st_as_sf(
    coords = c("long","lat"),
    crs = 4326
  )
bf_adm0 <- read_sf("/Users/ousmane/Documents/github-project/aez-memoire-code/data-processing/shapefile/bfa_adm_igb/bfa_adm0_igb.shp")
bf_adm1 <- read_sf("/Users/ousmane/Documents/github-project/aez-memoire-code/data-processing/shapefile/bfa_adm_igb/bfa_adm1_igb.shp")

ggplot() +
  geom_sf(data = bf_adm1, color = "grey",lwd = 0.3)+
  geom_sf(data =bf_adm0, color = "black", fill = NA, lwd = .8 )+
  geom_sf(data = std_norm9120_network)
  

# interpolation gstat 

v <- variogram(rain~1,std_norm9120_network)

v.fit <- fit.variogram(v,vgm(psill =10000,model = "Gau",nugget = 150)) 

# variogram fitted line (modeled)
df <- variogramLine(v.fit,maxdist = 320) %>% 
  as_tibble()

# variogram fit plot
ggplot()+
  geom_line(data = df, aes(x = dist, y = gamma),col = "red")+
  geom_point(data = v, aes(x=dist,y =gamma),col = "blue")

# interpolation grid
sf_use_s2(use_s2 = TRUE)
grid_points <- expand_grid(
  x=seq(-6,3,.001),
  y=seq(9,16,.001)
) %>% 
  st_as_sf(coords = c("x","y"),crs = 4326)
  



plot(grid_points)
# kriging model 

krige_model <- gstat(
  formula = rain ~ 1, 
  data = std_norm9120_network, 
  model = v.fit
  )
# Kriging prediction 

krige_res <- predict(krige_model, grid_points)

library(sp)
data("meuse.grid")
meuse.gridsf <- st_as_sf(meuse.grid, coords = c("x", "y"),
                       crs = 28992)
krige_res
ggplot(data = krige_res)+
  geom_sf(aes( color= var1.pred))
st_precision()
krige_res_raster <- krige_res %>% 
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>% 
  st_drop_geometry() %>%
  as_tibble() %>% 
  relocate(x,y,.before = everything()) %>% 
  terra::rast() %>% 
  `crs<-`(value = "EPSG:4326")
  
library(tidyterra)
ggplot()+
  geom_spatraster(data= krige_res_raster,aes(fill = var1.pred))+
  scale_fill_binned(
    breaks = c(600,900)
  )+
  geom_sf(data = bf_adm0,fill = NA,col = "red",lwd =1)
writeRaster(
  krige_res_raster["var1.var"],
  filename = "~/Desktop/climate_zoning/kriging_res_var.tif"
)

pred <- krige_res_raster["var1.pred"]
library(terra)

class <- c(0,600,1,600,900,2,900,1500,3) %>% 
  matrix(ncol = 3,byrow = 3)
pred.classified <- classify(pred,class,include.lowest = TRUE)

pred.classified.mask <- terra::mask(pred.classified,bf_adm0)

plot(pred.classified.mask)
terra::as.polygons(pred.classified.mask) %>% 
  st_as_sf() %>% 
  rename(code=var1.pred) %>% 
  mutate(name = c("sahelian","soudano-sahelian","soudanian")) %>% 
  write_sf("~/Desktop/climate_zoning/climate_zone.gpkg")
