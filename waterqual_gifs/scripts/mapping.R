#gonna try and make some maps with this!
rm(list=ls()) #clear env
source('scripts/data_cleaning.R')
library(rgdal)
library(tmap)

#load shape file for bay outline and wq datapoints
NB_area<-readOGR("data/NB_outline.shp")
SB_area<-readOGR("data/SB_outline.shp")
wq_shape<-wq%>% #let's just mess with one year first
  filter(year==2016)


#let's narrow the date range down
target_month<-8
wq_shape_t<-wq_shape%>%
  filter(month==target_month,year==target_year)
wq_shape_t<-wq_shape_t[complete.cases(wq_shape_t),]
nb_wq<-wq_shape_t%>%
  filter(region=="North Bay")
sb_wq<-wq_shape_t%>%
  filter(region=="South Bay")
#NB first
#set wq as a spatialpointdataframe
coordinates(nb_wq) = ~lon+lat

#set wq projection
proj4string(nb_wq)<-proj4string(NB_area)

nb_wq@bbox<-NB_area@bbox #set boundaries of wq_shape_t to bay_area boundaries

#lets try an IDW method
library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(nb_wq, "regular", n=200000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object


# Add wq's projection information to the empty grid
proj4string(grd) <- proj4string(nb_wq)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
wq.idw <- gstat::idw(m_sal ~ 1, nb_wq, newdata=grd, idp=2)

# Convert to raster object then clip to bay area
wq.r       <- raster(wq.idw)
wq.idr.nb     <- mask(wq.r, NB_area)

# Plot, this seems to be the interpolation!
breaks=c(0,4,8,12,16,20,24,28,32,36,40)
#jpeg('maps/nb_sal.png',width = 1200, height = 800)
tm_shape(wq.idr.nb ) + 
  tm_raster(breaks=breaks,palette = colorRampPalette(c("lightblue2", "lightsalmon"))(8), auto.palette.mapping = FALSE,
            title="Predicted salinity (ppt)") + 
  tm_shape(nb_wq) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)
#dev.off()

#SB second
#set wq as a spatialpointdataframe
coordinates(sb_wq) = ~lon+lat

#set wq projection
proj4string(sb_wq)<-proj4string(SB_area)

sb_wq@bbox<-SB_area@bbox #set boundaries of wq_shape_t to bay_area boundaries

#lets try an IDW method
library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(sb_wq, "regular", n=200000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object


# Add wq's projection information to the empty grid
proj4string(grd) <- proj4string(sb_wq)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
wq.idw <- gstat::idw(m_sal ~ 1, sb_wq, newdata=grd, idp=2.0)

# Convert to raster object then clip to bay area
wq.r       <- raster(wq.idw)
wq.idr.sb     <- mask(wq.r, SB_area)

# Plot, this seems to be the interpolation!
breaks=c(0,4,8,12,16,20,24,28,32,36,40)

jpeg('maps/sb_sal.png',width = 1200, height = 800)
tm_shape(wq.idr.sb ) + 
  tm_raster(breaks=breaks,palette = colorRampPalette(c("lightblue2", "lightsalmon"))(8), auto.palette.mapping = FALSE,
            title="Predicted salinity (ppt)") +
  tm_shape(sb_wq) + tm_dots(size=0.2) +
  tm_legend(legend.outside=TRUE)
dev.off()
