# identify water pixels on ridges
library(raster)

# set some things first

# tpi thresholds between classes
threshold1 <- -3  # threshold good/neutral
threshold2 <- 3   # threshold neutral/bad

# what cell value stands for water?
water_class <- 7

extract_method <- "simple"    # or "bilinear"

# define colors for colortable of final maps
# for selection see here: https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
color_good_water    <- "blue"       # good water
color_neutral_water <- "magenta3"   # neutral water
color_bad_water     <- "red3"       # suspicous water

filename_dem <- "C:/Dropbox (ScreenForBio)/GIS_Remote_sensing/GIS_data/raster_data/processing_data/SRTM/sabah/30m/n05_e117_1arc_v3.tif"
filename_landcover <- "C:/Dropbox (ScreenForBio)/GIS_Remote_sensing/GIS_data/raster_data/classifications/RapidEye_classifications_cloud_corrected (04_02_2014)/composite.img"

outfilename_water_raster     <- "C:/GIS_tempfiles/Sabah/classification_of_water_by_tpi/water_classified1.tif"
outfilename_landcover_raster <- "C:/GIS_tempfiles/Sabah/classification_of_water_by_tpi/landcover_water_classified1.tif"



### run from here  ###


# load dem
dem <- raster(filename_dem)

# load classification
landcov <- raster(filename_landcover)


# make tpi raster
tpi <- terrain(dem, opt='TPI')
# one could potentially also do this with a focal() call if larger neighbourhood is desired


# reclassify tpi values
# 0 = fine, 1 = neutral, 2 = suspicious
tpi_rcl <- reclassify(tpi, rcl = matrix(data = c(-1000, threshold1, 0,
                                                 threshold1, threshold2, 1,
                                                 threshold2, 1000, 2)))

# make binary water raster (1/0)
water <- landcov == water_class

# find water cells, get their coordinates, transform to tpi projection, extract tpi values (not used here) + tpi reclassified (which is used in this script)
which_cells_are_water <- which(values(water) == 1)
water_points          <- xyFromCell(water, cell = which_cells_are_water, spatial = TRUE)
water_points_latlong  <- spTransform(water_points, crs(tpi))

tpi_at_water_points     <- extract(tpi, water_points_latlong, method = extract_method)
tpi_rcl_at_water_points <- extract(tpi_rcl, water_points_latlong, method = extract_method)

# have a look at the values
summary(tpi_at_water_points)
table(tpi_rcl_at_water_points)


# define new water values for splitting water class
value_good_water    <- water_class * 10         # 7 -> 70
value_neutral_water <- (water_class * 10) + 1   # 7 -> 71
value_bad_water     <- (water_class * 10) + 2   # 7 -> 72

# reclassify water raster

water_classified <- water

values(water_classified)[which_cells_are_water][which(tpi_rcl_at_water_points == 0)] <- value_good_water
values(water_classified)[which_cells_are_water][which(tpi_rcl_at_water_points == 1)] <- value_neutral_water
values(water_classified)[which_cells_are_water][which(tpi_rcl_at_water_points == 2)] <- value_bad_water

plot(water_classified)

# reclassify water in original classification
water_values_reclassified <- ifelse(tpi_rcl_at_water_points == 0, value_good_water,
                                    ifelse(tpi_rcl_at_water_points == 1, value_neutral_water,
                                           ifelse(tpi_rcl_at_water_points == 2, value_bad_water, -9999)))


values(landcov)[which_cells_are_water] <- water_values_reclassified




color_code_good_water <- rgb(t(col2rgb(color_good_water)), maxColorValue = 255)
color_code_neutral_water <- rgb(t(col2rgb(color_neutral_water)), maxColorValue = 255)
color_code_bad_water <- rgb(t(col2rgb(color_bad_water)), maxColorValue = 255)

# set nice colors in raster colortable
# + 1 because 1st values is for color at value 0
colortable(landcov)
colortable(landcov) <- colortable(landcov)
colortable(landcov)[value_good_water + 1]    <- color_code_good_water
colortable(landcov)[value_neutral_water + 1] <- color_code_neutral_water
colortable(landcov)[value_bad_water + 1]     <- color_code_bad_water

colortable(water_classified) <- rep("#000000", times = 256)
colortable(water_classified)[value_good_water + 1]    <- color_code_good_water
colortable(water_classified)[value_neutral_water + 1] <- color_code_neutral_water
colortable(water_classified)[value_bad_water + 1]     <- color_code_bad_water

# save rasters
writeRaster(water_classified, filename = outfilename_water_raster, format = "GTiff", datatype = "INT1U")
writeRaster(landcov, filename = outfilename_landcover_raster, format = "GTiff", datatype = "INT1U")

