library(rgdal)
library(rgeos)
library(raster)
#Lab 1 Answer Key

setwd("Dropbox/course/g605W/labs/data/kmv")
vdc <- readOGR(dsn='./', layer='kmv_vdc', stringsAsFactors=FALSE) #may need '
precip <- readOGR(dsn='./Precip_data/shapefiles', layer='vdc_precip', stringsAsFactors=FALSE) #may need '
agric <-  raster("scasia_v4_grid/nepagric")
forest <-  raster("scasia_v4_grid/nepforest")
grass <-  raster("scasia_v4_grid/nepgrasslands")
irrigated <-  raster("scasia_v4_grid/nepirrigated")

proj4string(vdc) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
proj4string(precip) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

vdc.tm <- spTransform(vdc, CRS(proj4string(agric)))
precip.tm <- spTransform(precip, CRS(proj4string(agric)))

bounds <- gUnionCascaded(vdc.tm)
boundsSP <- SpatialPolygonsDataFrame(bounds, data.frame(id=1:length(bounds)))
agricKMV <- crop(agric, boundsSP)
forestKMV <- crop(forest, boundsSP)
grassKMV <- crop(grass, boundsSP)
irrigatedKMV <- crop(irrigated, boundsSP)


#Check what districts there are:
unique(precip.tm$DISTRICT)

#what districts are in kathmandu valley?, google says Bhaktapur, Kathmandu and Lalitput
#or just check the other object:
unique(vdc$DISTRICT)

precipKMV <- subset(precip.tm, DISTRICT %in% c("Lalitpur", "Kathmandu", "Bhaktapur"))
#or if we want to be fancy
precipKMV <- subset(precip.tm, DISTRICT %in% unique(vdc$DISTRICT))

#check the results
length(precipKMV) == length(vdc) #are there the same number of features in each?
#create a raster stack so only have to do extract once
LUstack <- stack(agricKMV, forestKMV, grassKMV, irrigatedKMV)
precipKMV.LU <- extract(LUstack, precipKMV, sp=TRUE, fun=sum, na.rm=TRUE)

#check results
head(precipKMV.LU@data)
#the column indexes with the LU data are at the end, we can check with (see if you can figure out whats going on here)
head(precipKMV.LU@data[,(length(names(precipKMV.LU))-3):length(names(precipKMV.LU))])
#we can find the column index of the max LU category
max.col(precipKMV.LU@data[,(length(names(precipKMV.LU))-3):length(names(precipKMV.LU))])
#and dump it into a new column
precipKMV.LU$domLU <- max.col(precipKMV.LU@data[,(length(names(precipKMV.LU))-3):length(names(precipKMV.LU))])
#check to make sure its correct
head(precipKMV.LU@data[,(length(names(precipKMV.LU))-4):length(names(precipKMV.LU))])

#looks good, recast as LU codes
precipKMV.LU$LUC <- ""
precipKMV.LU$LUC[which(precipKMV.LU$domLU == 1)] <- "A"
precipKMV.LU$LUC[which(precipKMV.LU$domLU == 2)] <- "F"
precipKMV.LU$LUC[which(precipKMV.LU$domLU == 3)] <- "G"
precipKMV.LU$LUC[which(precipKMV.LU$domLU == 4)] <- "I"

library(ggplot2)
library(reshape2)
library(plyr)
dfm <- ddply(precipKMV.LU@data, .(LUC), summarise, avgPrecip = mean(JAN_MEAN)) #summarize by mean
#check 
mean(precipKMV.LU$JAN_MEAN[which(precipKMV.LU$LUC == "A")])
#vs
dfm
#now plot it
g <- ggplot(dfm, aes(x=LUC, y=avgPrecip)) + geom_bar(stat='identity')
g
#but it might be cool to look at all months also
#lets first get rid of some columns:
df <- precipKMV.LU@data[,c(14:length(names(precipKMV.LU)))]
df <- df[,! names(df) %in% c('nepagric', 'nepforest', 'nepirrigated', 'nepgrasslands', 'domLU')] #drop some columns
#the data - in its current form is in 'wide' format, meaning multiple columns, we need to convert this to 'long' format using the reshape packge functions for manipulating data
df2 <- melt(df, id.vars=c("VDC_NAME_1","LUC"))
#take a look
head(df2)
dim(df2)
dfm <- ddply(df2, .(variable, LUC), summarise, avgPrecip = mean(value)) #summarize by mean
g <- ggplot(dfm, aes(x=LUC, y=avgPrecip)) + geom_bar(stat='identity')
g + facet_grid(. ~ variable)

#or we could look at variability across VDCs in each LU code - so not averaging
g <- ggplot(df2, aes(x=LUC, y=value)) + geom_boxplot()
g + facet_grid(. ~ variable)

#and pretty it up a bit
g + facet_grid(. ~ variable) + xlab("Landuse Type") + ylab("Precipitation (mm)")

#now we might want to see how precipitation varies spatially, it is obvious that the summer monsoon months have the most variability - so we can focus on one of those months
plot(precipKMV.LU)
#indices from 15-26
precipKMV.LU$devP <- precipKMV.LU$AUG_MEAN - mean(precipKMV.LU$AUG_MEAN)
#map the residuals of August mean rainfall
spplot(precipKMV.LU, zcol="devP", pretty=TRUE)
