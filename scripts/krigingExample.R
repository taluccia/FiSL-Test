dt<-tibble::tribble(
  ~id,  ~lon,  ~lat,    ~z,
  "a",  1.1,   1.3,  12.5,
  "b",  2.4,   1.7,  13.0,
  "c",  3.2,   1.4,  12.0,
  "d",  4.2,   1.4,  16,
  "e",  1.2,   2.3,  13.0,
  "f",  2.3,   2.7,  15.5,
  "g",  3.7,   2.5,  19.0,
  "h",  4.5,   2.2,  17.5,
  "i",  1.1,   3.2,  16.5,
  "j",  2.2,   3.4,  18.5,
  "k",  3.7,   3.3,  18.2,
  "l",  4.7,   3.3,  11.5,
  "m",  1.1,   4.1,  17.5,
  "n",  2.2,   4.2,  18.5,
  "o",  3.7,   4.3,  19.2,
  "p",  4.7,   4.2,  8  
)

DT_sf  <-  st_as_sf(dt, coords = c("lon", "lat"), 
                    crs = 4326, agr = "constant")

ggplot() +
  geom_sf(data=DT_sf, aes(color=z), size=10)


DT_sp  <- as_Spatial(DT_sf)
Now I need to create a grid of points I would like to predict the values for.

lon <- seq(1.0, 5.0, length.out = 100)
lat <- seq(1.0, 5.0, length.out = 100)
grd <- expand.grid(lon = lon, lat = lat)

grd_sf  <-  st_as_sf(grd, coords = c("lon", "lat"), 
                     crs = 4326, agr = "constant")

grd_sp <- as_Spatial(grd_sf)
Now I need to create a variogram. Like in lm values can be dependent on some feature like distance from the river (meuse dataset) and if there is no such variable, use 1.

dt.vgm <- variogram(z~1, DT_sp)

class(dt.vgm)

dt.fit <-
  fit.variogram(dt.vgm, model = vgm(1,"Lin",900,1)) # fit model

# vgm() list of models

plot(dt.vgm, dt.fit)
Now I can perform kriging and plot the result

lzn.kriged <- krige((z) ~ 1, DT_sp, grd_sp, model=dt.fit)

lzn.kriged

lzn.kriged %>% as.data.frame %>% rename(lon=coords.x1, lat=coords.x2) %>% 
  ggplot(aes(x=lon, y=lat)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient2(low="green", mid = "yellow",  high="red",midpoint = 15) +
  theme_bw()+
  geom_point(data=dt, aes(color=z), size=10)+
  geom_text(data=dt, aes(label=z), color="white")
