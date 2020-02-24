library(rgdal)


# http://epsg.io/3168-8659
projection_string_rso <- CRS("+proj=omerc +lat_0=4 +lonc=102.25 +alpha=323.0257905 +k=0.99984 +x_0=804670.24 +y_0=0 +no_uoff +gamma=323.1301023611111 +a=6377295.664 +b=6356094.667915204 +units=m +no_defs")
projection_string_utm <- CRS("+proj=utm +zone=48 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
projection_string_latlong <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# make coordinate data frame. Needs columns x and y
coordinate_dataframe <- data.frame(x = 525369, y = 542696)

# do the translation
# http://www.asprs.org/a/resources/grids/04-2009-malaysia.pdf
coordinate_dataframe_translated   <- coordinate_dataframe
coordinate_dataframe_translated$x <- coordinate_dataframe$x + 11
coordinate_dataframe_translated$y <- coordinate_dataframe$y - 851


# change this according to your data
layer_attributes <- data.frame(name = "Kenyir ranger station")


# maek spatialPointsDataFrame
pts_rso_translate <- SpatialPointsDataFrame(coords      = coordinate_dataframe_translated,
                                            proj4string = projection_string_rso,
                                            data        = layer_attributes)



# transform to lat/long
pts_latlong_translate <- spTransform(pts_rso_translate, CRSobj = projection_string_latlong)

#plot(pts_latlong_translate)

outdir <- "M:/collaborations/John/geodata_conversion"    # change this



writeOGR(obj = pts_latlong_translate,
         dsn = file.path(outdir, paste("points_latlong_translate", Sys.Date(), ".kml", sep = "")),
         layer =  "points_latlong",
         driver = "KML"
)

# the kml can be opened in Google Earth
