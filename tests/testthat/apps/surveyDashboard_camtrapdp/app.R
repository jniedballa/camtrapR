
library(camtrapR)

path_camtrapdp <- system.file("sample_data/tdwg_camtrap-dp_1.0.2_example", 
                                    package = "camtrapR")
camtrapdp <- readCamtrapDP(file = file.path(path_camtrapdp, "datapackage.json")) 


# Call shiny app function
surveyDashboard( CTtable = camtrapdp$CTtable,
                 recordTable = camtrapdp$recordTable, 
                 xcol = "longitude", 
                 ycol = "latitude", 
                 crs = 4326, 
                 stationCol = "locationName", 
                 setupCol = "Setup_date", 
                 retrievalCol = "Retrieval_date", 
                 CTdateFormat = "ymd HMS", 
                 speciesCol = "vernacularName_eng")
