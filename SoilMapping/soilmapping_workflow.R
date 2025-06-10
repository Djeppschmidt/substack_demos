
#####################################
# soil data mapping pilot workflow
#####################################

# Make sure you are in working directory of the github repository.
# setwd("/Users/dietrich/Documents/GitHub/substack_demos/SoilMapping/") <- my directory path
# setwd("[Path/to/Directory]/substack_demos/SoilMapping/")

# install libraries
packages <- c("raster", "stars", "sf", "soilDB", "leaflet", "elevatr")
install.packages(setdiff(packages, rownames(installed.packages()))) 

# load libraries

library(raster)
library(stars)
library(sf)
library(soilDB)
library(leaflet)
library(elevatr)

# set t1 for workflow time benchmark
t1 <- Sys.time()

# pull raster layer from SOLUS100
# depth sclices in this case is only 0-5cm
# soil factor is pH
layers<-raster(soilDB::fetchSOLUS(depth_slices = c("0"), 
                                   variables = c("ph1to1h2o")))

# Read in geometry file
mpfile<-list.files(pattern="\\.json$") # find geometry file
mp=sf::read_sf(mpfile)

# select counties for map geometry:
# subset mp to only contiguous states
# 54 = WV; PA = 42; VA = 51; MD = 24; NY = 36; NC = 37; VT = 50; KY = 21; TN = 47

mp<-mp[mp$STATE=="54"| 
         mp$STATE=="42" | 
         mp$STATE=="51" | 
         mp$STATE=="24" | 
         mp$STATE=="36" | 
         mp$STATE=="37" | 
         mp$STATE=="50" | 
         mp$STATE=="21" | 
         mp$STATE=="47"
       ,] # start with state codes

# for a single state view, in this case WV:
# I use one state for quicker computation when testing
mp<-mp[mp$STATE=="54"
       ,]

# crop the raster file to borders
mp = st_transform(mp, st_crs(layers)) # datum transformation
sf_use_s2(FALSE) # turn off s2 (curve projection)

## crop and mask
r2 <- crop(layers, extent(mp))
r3 <- mask(r2, mp)

# define color palette
pal2<-colorNumeric("RdYlBu", getValues(r3), na.color = "transparent")

# plot condition with colors
# a common error is maxBytes is too small for number of pixels plotting. If this is the case, the error will tell you how many bytes you need, and you can set the max above that value. The default is way too low for most regional plotting.
leaflet() %>% addTiles() %>%
  addRasterImage(r3, pal2, opacity = 0.6, maxBytes = 10000000) %>% addLegend(pal = pal2, values = getValues(r3), position = "topleft")

# check time to run workflow:
Sys.time()-t1 # 1.036 minutes on a 2018 mac mini; 3.2 ghz 6 cores

###########################################
# map conditions within range
##########################################

# start time benchmark for part 2
t2 = Sys.time()

# define a raster analysis of single layers:
# this is defined for overlay, will need to

habitat.v1 = r3 > 6.9
habitat.v1[habitat.v1 == 0] <- NaN

# define color palette for overlay
pal <- colorRampPalette(c("purple"))
pal<-colorNumeric(pal = pal, domain = c(1), na.color = "transparent")


leaflet() %>% addTiles() %>%
  addRasterImage(habitat.v1, colors = pal, opacity = 0.6, maxBytes = 10000000)

# check time
Sys.time() - t2 # 46.2 seconds on a 2018 mac mini; 3.2 ghz 6 cores

#####################################
# Extended analysis: map find habitat from multiple layers
#####################################

# define a raster analysis of multiple layers:
# NOTE FOR DASHBOARD: in order to turn this into a dashboard with reactive values, function will need to be restructured to operate independently on each layer (input is layer, critera); then in a second step combine all input layers selected for analysis with the overlay function

Habitat02<-function(pH.layer, slope.layer){
  # define thresholds
  pH.condition = pH.layer > 6.9
  slope.condition = slope.layer < 6
  
  # combine conditions. Values are 1 if all criteria are met, and 0 if not
  out <- pH.condition * slope.condition
  
  # set zeros to NAN; this will make them transparent when plotting
  out[out == 0] <- NaN
  
  # define output of function
  return(out)
}

# start time benchmark
t3 = Sys.time()

# define slope raster
elevation <- get_elev_raster(mp, z = 11) # set z to lower
slope <- terrain(elevation, opt = "slope", unit = "degrees")
slope_res <- raster::resample(slope, r3, method = "bilinear")
slope_res<-crop(slope_res, extent(mp))
slope_res<-mask(slope_res, mp)

# calculate habitat condition 
habitat.v2 <-overlay(r3, 
                  slope_res, 
                  fun=Habitat02)

# plot habitat conditions
leaflet() %>% addTiles() %>%
  addRasterImage(habitat.v2, 
                 colors = pal, # use purple as defined previously
                 opacity = 0.6,
                 maxBytes = 10000000)

# check runtime for last portion
Sys.time() - t3 # 6.35 minutes on a 2018 mac mini; 3.2 ghz 6 cores