
# Derive a stream network from a DEM using QGIS and R
# Author: Jürgen Niedballa
# Date: 3.8.2017


# software requirements:
#    - QGIS with grass7 installed
# data requirements:
#    - a void-filled digital elevation model, the script assumes 30x30m. It does not require hydrological correction.
#    - country-level administrative shapefiles fo get accurate coastline information for clipping the DEM and remove the ocean
# data output:
#    - a stream raster in the same resolution / extent as the DEM

library(raster)
library(rgdal)
library(rgeos)
library(camtrapR)

# add QGIS directory to PATH temporily (so the R system command can find the QGIS tools)
exiftoolPath("C:/Program Files/QGIS 2.18/bin")
# alternatively add this directory system PATH variable


# directory to save output in
dir_output <- "C:/GIS_tempfiles/DEM_streams"

# load dem
dir_dem  <-  "C:/Dropbox (ScreenForBio)/GIS & Remote sensing/GIS_data/raster_data/processing_data/SRTM/vietnam/30m"
filename_dem <- "SRTM_30m_mosaic_Vietnam_Laos_void_filled.tif"
file_dem_original <- file.path(dir_dem, filename_dem)
r_orig <- raster(file_dem_original)





################################################################
# prepare DEM by removing the ocean (ocean was 0, should be NA to prevent rivers in there)
################################################################

# this is achieved using a admin shapefile combining vietnam and laos
# then this admin shapefile is rasterized and used to remove the ocean in the DEM

#  load admin shapefiles
dir_admin_vn <-  "C:/Dropbox (ScreenForBio)/GIS & Remote sensing/GIS_data/shapefiles/Vietnam/admin"
dir_admin_lao <- "C:/Dropbox (ScreenForBio)/GIS & Remote sensing/GIS_data/shapefiles/Laos/admin"

filename_admin_vn  <- "VNM_adm0"
filename_admin_lao <- "LAO_adm0"

admin_vn <- readOGR(dsn = dir_admin_vn, layer = filename_admin_vn)
admin_lao <- readOGR(dsn = dir_admin_lao, layer = filename_admin_lao)

# union both
vietnam_laos <- union(admin_vn, admin_lao)
#plot(vietnam_laos)

# overwrite unused attribute table with same value for both countries (we will later multiply this admin raster with the DEM, so we want only values 1 and NA)
vietnam_laos@data <- data.frame(VALUE = c(1,1))   # remove attribute table and remplace with simple value 1 for rasterizing

# save union shapefile
writeOGR(obj = vietnam_laos,
         dsn = dir_output,
         layer = "admin_vietnam_laos_merged",
         driver = "ESRI Shapefile")

# create mask for rasterizing in QGIS (can be done in QGIS or from R with a system call)
# this mask only contains values 1 and NA.
# by multiplying with DEM, we get either the original DEM values (on land) or NA (in the ocean)

# create mask raster from dem and save it
r_mask <- r_orig
values(r_mask) <- NA
writeRaster(x = r_mask, filename = file.path(dir_output, "ADMIN_rasterize_mask.tif"), datatype = "INT1U")

# rasterize admin shapefile
# this is done in QGIS (gdal_rasterize) because it is much faster than R:raster:rasterize
# gdal_rasterize takes an input raster and changes it. It doest not create a new raster. So we have to provide the mask first which is easy in R:raster

# either do it manually in QGIS, Raster -> Conversion -> rasterize

# or run gdal_rasterize from within R

# for this, define filesnames first
filename_admin <- "admin_vietnam_laos_merged"    # name of input shapefile
filename_admin_full <- file.path(dir_output, "admin_vietnam_laos_merged.shp")   # full path of input shapefile (incl. file extension)
filename_admin_raster_full <- file.path(dir_output, "ADMIN_rasterize_mask.tif")  # full path of raster

# create and run system call (gdal_rasterize must be in your PATH)
system(paste("gdal_rasterize -a VALUE -l", filename_admin, filename_admin_full, filename_admin_raster_full))


# load updated admin raster back into R
r_mask_admin <- raster(file.path(dir_output, "ADMIN_rasterize_mask.tif"))
#table(values(r_mask_admin))   # check if it worked

r_clip <- r_orig * r_mask_admin     # multiply mask and DEM to make ocean = NA
plot(r_clip)        # :) yeah!

# save DEM with ocean = NA
filename_dem_clipped_out <- "SRTM_30m_mosaic_Vietnam_Laos_void_filled_clipped.tif"
writeRaster(r_clip, filename = file.path(dir_output, filename_dem_clipped_out), datatype = "INT2S")



################################################################
# calculate flow on DEM
################################################################

# run r.terraflow in QGIS with the clipped output raster
# be sure to produce filled DEM, flow direcion, flow accumulation rasters
# use D8 algorithm (equivalent to ArcGIS), or play around with MFD (probably slower).
# it takes quite a while to process a large DEM
# this part may also be called from R e.g. with rgrass7, but I didnt try yet

# IMPORTANT: be sure to check in GIS if flow accumulation raster is consistent!!!!! I.e. if we have a proper  gapless stream network


# load flow accumulation raster produced in QGIS and produce different stream rasters

filename_flowaccu <- file.path(dir_output, "qgis_workflow", "r.terraflow", "flowaccu.tif")
flowaccu <- raster(filename_flowaccu)
plot(flowaccu)

# find number of cells per minimum catchment area
pixelsize <- 30   # in meters
n_cells_per_ha  <- floor(1 / (pixelsize/100)^2)   # n cells in 1 ha
n_cells_per_km2 <- floor(1 / (pixelsize/1000)^2)  # n cells in 1 km2


# define streams using minimum catchment area to create stream rasters (be free to use other thresholds)
stream_raster_1km2 <- flowaccu >= n_cells_per_km2
stream_raster_1ha  <- flowaccu >= n_cells_per_ha

# stream_raster_10ha  <- flowaccu >= 10 * n_cells_per_ha    # example for 10 ha min catchment size. analogously for other sizes
# stream_raster_10km2  <- flowaccu >= 10 * n_cells_per_km2    # example for 10 ha min catchment size. analogously for other sizes


# if required set non-streams to NA with this command
#values(stream_raster_1ha)[which(values(stream_raster_1ha == 0))] <- NA

# save stream rasters
writeRaster(stream_raster_1km2, filename = file.path(dir_output, "stream_raster_1km2.tif"), datatype = "INT1U")
writeRaster(stream_raster_1ha,  filename = file.path(dir_output, "stream_raster_1ha.tif"),  datatype = "INT1U")




# now the unfortunate part:
# in ArcGIS there is a nice command called Streams to Feature which converts a stream raster into a stream shapefile with the help of the flow direction raster
# i haven't yet found this algorithm in QGIS
# so the best we have is the stream raster.
# But if we calculate the distance from the camera trap stations to the xy coordinate of each stream raster pixel, we should get almost the same results unless cameras were very close to rivers

# question: can GRASS flow direction be used in ArcGIS?
# only when using SFD (single flow direction) in r.terraflow, not when using MFD (multiple flow dir)
# even then, flow direction raster needs reclassification

# in ArcGIS: https://pro.arcgis.com/de/pro-app/tool-reference/spatial-analyst/how-flow-direction-works.htm   # range from 1 ... 128
# in r.terraflow values range from 0 ... 8
# -> would require reclassification.
# easy in theory, but i dont know which direction in r.terraflow corresponds to what cardinal direction (can be found out easilty if necessary)



#################################################################################################
# calculate distance between camera traps and nearest water pixel (this is a draft!!!)
# adapt by using the desired stream raster with appropriate stream definition (minimum catchment area, as set above)
# this can be done with a list to use less code
# my suggestion: use log scale, e.g. 0.1, 1, 10, 100 km2, but by all means let your ecological understanding guide you.
# use the flow accumulation raster to find catchment sizes of river stretches you know
# (remember, unit of flowAccu if number of upstream cells, correlated to an area but not an area in iself)


# load camera traps
#####################!!!!!!!!!!!!!!!!!!!!!!################
# UTM_E and UTM_N need to be swapped. Please change!

CT_table_course <- read.csv("C:/Dropbox (ScreenForBio)/Projects/camtrapR/_feedback_questions/Andrew/data_tables_june17_course.csv", stringsAsFactors = FALSE)

# get xy coordinates of river cell centres
xy_river_tmp <- xyFromCell(object = stream_raster_1km2, cell = which(values(stream_raster_1km2) == 1 ))
# convert this coordiante matrix to spatial object for distance computation
sp_river_tmp <- SpatialPoints(xy_river_tmp, proj4string=CRS(as.character("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")))

# make spatial point data frame of camera trap locations
ct_spdf <- SpatialPointsDataFrame(data.frame(x = CT_table_course$UTM_E, y = CT_table_course$UTM_N),
                                  proj4string = CRS("+proj=utm +zone=48 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "),
                                  data = CT_table_course)

plot(ct_spdf)   # if this looks wrong fix UTM_E and UTM_N in input table!!!!!!!

# transform xy of river cells (which is lat long) to UTM
sp_river_tmp <- spTransform(sp_river_tmp, CRSobj = CRS("+proj=utm +zone=48 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))

# calculate distances from all camera traps to all river cells
dist_tmp <- gDistance(ct_spdf, sp_river_tmp, byid = TRUE)

# find minumum distance from each station
min_dist <- round(apply(dist_tmp, MARGIN = 2, FUN = min))

# store this value in original data frame
CT_table_course$dist_water_1km2 <- min_dist

# SAVE TABLE!!! OR Shapefile


hist(CT_table_course$dist_water_1km2)
rug(CT_table_course$dist_water_1km2)


########################################
# these are the commands used in ArcGIS to achieve the same

# in ArcGIS:
# 1. Fill
# 2. Flow Direction
# 3. Flow Accumulation
# 4. Reclassify  (set threshold to reflect minimum catchment area): 1 pixel = 30x30m = 900m² = 0.09 ha    # can easlily be done in R
# 1 km² = 1111 cells, 1 ha = 11 cells
# maybe make ocean = NA
# 5. Stream To Feature




#######################################################################################
#######################################################################################
#
# everything below is just trials with the RQGIS package. Ignore
#
#######################################################################################
#######################################################################################

# according to Pierre RQGIS only works with QGIS commands, not other toolboxes like grass
# possible alternative: rgrass7 package in R

library("RQGIS")
library("raster")
library("rgdal")



##########



# define directories
dir_QGIS <- "C:/Program Files/QGIS 2.18"
dir_dem  <-  "C:/Dropbox (ScreenForBio)/GIS & Remote sensing/GIS_data/raster_data/processing_data/SRTM/vietnam/30m"
filename_dem <- "SRTM_30m_mosaic_Vietnam_Laos_void_filled.tif"

file_dem_original <- file.path(dir_dem, filename_dem)


filename_dem_crop <- "SRTM_30m_mosaic_Vietnam_Laos_void_filled_crop_test.tif"


# crop DEM for testing purposes

file_dem <- file.path(dir_dem, filename_dem)
r <- raster(file_dem)
r_crop <- crop(r, y = extent(r, 1,1000,1,1000))   # take row and column 1:1000
writeRaster(r_crop, filename = file.path(dir_output, filename_dem_crop), datatype = "INT2S")


# decive which file to work on, original or cropped
#file_dem <- file_dem_crop     # alternative: file_dem_original


# set QGIS directory
dir_QGIS <- "C:/OSGeo4W64"
dir_QGIS <- "C:/Program Files/QGIS 2.18"
set_env(root = dir_QGIS)


library(camtrapR)
exiftoolPath("C:/Program Files/QGIS 2.18/apps/grass/grass-7.0.5/bin")
exiftoolPath("C:/Program Files/QGIS 2.18/apps/grass/grass-7.0.5/lib")


"C:\Program Files\QGIS 2.18\apps\grass\grass-7.0.5\bin\r.terraflow.exe"




################################################################
# call r.terraflow function which does all at once
################################################################

find_algorithms(search_term = "terraflow")
alg_terraflow <- "grass7:r.terraflow"   # name of the algorithm used

get_usage(alg = alg_terraflow)

# define functioni parameters
params <- get_args_man(alg = alg_terraflow)
params

params$elevation <- file_dem
params$`-s` <- "True"
params$direction <- file.path(dir_output, "terraflow_flowdir.tif")


terraflow <- run_qgis(alg = alg_terraflow,
                       params = params,
                       load_output = TRUE)




################################
# create depressionless DEM
################################


find_algorithms(search_term = "fill")

alg_fill <- "saga:fillsinks"   # name of the algorithm used

get_usage(alg = alg_fill)

# define functioni parameters
params <- get_args_man(alg = alg_fill)
params

params$DEM <- file_dem
params$RESULT <- file.path(dir_output, "DEM_depressionless.tif")

DEM_filled <- run_qgis(alg = alg_fill,
                       params = params,
                       load_output = TRUE)


######
# alternative, also produced flow direction in one go (seems to work better)

alg_fill2 <- "grass7:r.fill.dir"
get_usage(alg = alg_fill2)

# define functioni parameters
params <- get_args_man(alg = alg_fill2)
params

params$input  <- file_dem
params$output <- file.path(dir_output, "DEM_depressionless_grass.tif")
params$direction <- file.path(dir_output, "flowdirection_grass.tif")
params$areas <- file.path(dir_output, "problem_areas_grass.tif")

DEM_filled_grass <- run_qgis(alg = alg_fill2,
                             params = params,
                             load_output = TRUE)



#######################################
# aspect
#######################################

find_algorithms(search_term = "aspect")
alg_aspect <- "saga:slopeaspectcurvature"

get_usage(alg = alg_aspect)

# define functioni parameters
params <- get_args_man(alg = alg_aspect)
params

params$ELEVATION <- file.path(dir_output, "DEM_depressionless_grass.asc")
params$METHOD <- "6"
params$UNIT_SLOPE <- "0"   # radians
params$UNIT_ASPECT <- "0"  # radians
params$SLOPE <- file.path(dir_output, "slope.tif")
params$ASPECT <- file.path(dir_output, "aspect.tif")


slope_aspect_grass <- run_qgis(alg = alg_aspect,
                               params = params,
                               load_output = TRUE)



#######################################
# flow direction and accumulation
#######################################

find_algorithms(search_term = "watershed")

alg_flow <- "grass:r.watershed"
get_usage(alg = alg_flow)

params <- get_args_man(alg = alg_flow)
params

params$elevation <- file.path(dir_output, "DEM_depressionless_grass.tif")
params$`-f` <- "True"    # True = D8 flow direction, false = multiple flow direction
params$accumulation <- file.path(dir_output, "flowaccu.tif")
params$accumulation <- "C:\\GIS_tempfiles\\DEM_streams\\flowaccu.tif"
params$threshold <- 1111


flowaccu <- run_qgis(alg = alg_flow,
                     params = params,
                     load_output = TRUE)


# find_algorithms(search_term = "accumulation")
#
# alg_flow <- "grass7:r.flow"     # this one needs projected data. For use with latlong, use r.watershed
# get_usage(alg = alg_flow)
#
# params <- get_args_man(alg = alg_flow)
# params
#
# params$elevation <- file.path(dir_output, "DEM_depressionless_grass.tif")
# params$flowline <- file.path(dir_output, "flowline")
# params$flowaccumulation <- file.path(dir_output, "flowaccu.tif")
# params$flowlength <- file.path(dir_output, "flowlength.tif")
#
#
# flowaccu <- run_qgis(alg = alg_flow,
#                      params = params,
#                      load_output = TRUE)



alg_catchmentarea <- "saga:catchmentarea"

get_usage(alg = alg_catchmentarea)

# define functioni parameters
params <- get_args_man(alg = alg_catchmentarea)
params

params$ELEVATION <- file.path(dir_output, "DEM_depressionless_grass.tif")
params$CAREA <- file.path(dir_output, "saga_catchment_area.tif")

params$ELEVATION <- "C:/GIS_tempfiles/DEM_streams/DEM_depressionless.tif"
params$CAREA <- "C:/GIS_tempfiles/DEM_streams/qgis_workflow/saga_catchment_area.tif"


catchment_saga <- run_qgis(alg = alg_catchmentarea,
                           params = params,
                           load_output = TRUE)




# in TauDEM

find_algorithms(search_term = "taudem")

alg_flowdir <- "taudem:d8flowdirections"

get_usage(alg = alg_flowdir)

params <- get_args_man(alg = alg_flowdir)
params

params$`-fel` <- file.path(dir_output, "DEM_depressionless_grass.tif")
params$`-p` <- file.path(dir_output, "test1.tif")
params$`-sd8` <- file.path(dir_output, "test2")
# params$accumulation <- "C:\\GIS_tempfiles\\DEM_streams\\flowaccu.tif"
# params$threshold <- 1111


flowdir <- run_qgis(alg = alg_flowdir,
                    params = params,
                    load_output = TRUE)



# Alternatives

# https://gis.stackexchange.com/questions/84694/flow-accumulation-in-r

#library(spgrass6)
library(rgrass7)

#TauDEM (via command line)
#http://hydrology.usu.edu/taudem/taudem5/TauDEMRScript.txt


#RSAGA: läuuft mit kommandozeile
#noch besser: SAGA über kommandozeile aus R aufrufen