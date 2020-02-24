## -----------------------------------------------------------------------------
library(camtrapR)
data(camtraps)

## ----eval = FALSE-------------------------------------------------------------
#  
#  # find data for 2 species with correctly spelled names
#  checkNames1 <- checkSpeciesNames (speciesNames = c("Bearded Pig", "Malayan Civet"),
#                                    searchtype   = "common")
#  checkNames1

## ----echo = FALSE-------------------------------------------------------------

structure(list(tsn = c(625012, 622004), user_name = structure(1:2, .Label = c("Bearded Pig", 
"Malayan Civet"), class = "factor"), scientificName = c("Sus barbatus", 
"Viverra tangalunga"), commonName = c("bearded pig/Bearded Pig", 
"Malayan Civet"), authorship = c("Müller, 1838", "Gray, 1832"
), rankname = c("Species", "Species"), itis_url = c("https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=625012", 
"https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=622004"
), taxon_status = c("valid", "valid")), row.names = c(NA, -2L
), class = "data.frame")

## ----eval = FALSE-------------------------------------------------------------
#  # and with scientific names (note that this is for a subspecies)
#  checkNames2 <- checkSpeciesNames (speciesNames = "Viverra tangalunga tangalunga",
#                                    searchtype   = "scientific")
#  checkNames2

## ----echo = FALSE-------------------------------------------------------------
structure(list(tsn = 726578, user_name = structure(1L, .Label = "Viverra tangalunga tangalunga", class = "factor"), 
    scientificName = "Viverra tangalunga tangalunga", commonName = NA, 
    authorship = "Gray, 1832", rankname = "Subspecies", itis_url = "https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=726578", 
    taxon_status = "valid"), row.names = c(NA, -1L), class = "data.frame")

## ----eval = FALSE-------------------------------------------------------------
#  # an invalid name: the accepted name of the leopard cat is Prionailurus bengalensis
#  checkNames3 <- checkSpeciesNames (speciesNames = "Felis bengalensis",
#                                    searchtype   = "scientific",
#                                    accepted     = FALSE)
#  checkNames3

## ----echo = FALSE-------------------------------------------------------------
structure(list(tsn = 183793, user_name = structure(1L, .Label = "Felis bengalensis", class = "factor"), 
    scientificName = "Felis bengalensis", commonName = "leopard cat", 
    authorship = "Kerr, 1792", rankname = "Species", itis_url = "https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=183793", 
    taxonUsageRating = "invalid"), class = "data.frame", row.names = c(NA, 
-1L))

## ----eval = FALSE-------------------------------------------------------------
#  # an ambiguous name name: Chevrotain (Tragulus)
#  checkNames4 <- checkSpeciesNames (speciesNames = "Chevrotain",
#                                    searchtype   = "common")
#  1              # making a choice from the menu
#  checkNames4

## ----echo = FALSE-------------------------------------------------------------
structure(list(tsn = 624919, user_name = structure(1L, .Label = "Chevrotain", class = "factor"), 
    scientificName = "Tragulidae", commonName = "chevrotains", 
    authorship = "Milne-Edwards, 1864", rankname = "Family", 
    itis_url = "https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=624919", 
    taxon_status = "valid"), row.names = c(NA, -1L), class = "data.frame")

## -----------------------------------------------------------------------------

# this dummy directory will be used as inDir (containing station directories with species subdirectories)
wd_createSpeciesFoldersTest <- file.path(tempdir(), "createSpeciesFoldersTest")

# now first create the station directories
# (normally, you'd create species directories in the station directories that 
# already contain renamed, unsorted images). Again, this is for demonstation only.
StationFolderCreate1 <- createStationFolders (inDir       = wd_createSpeciesFoldersTest,
                                              stations    = as.character(camtraps$Station), 
                                              createinDir = TRUE)
StationFolderCreate1


# species names for which we want to create subdirectories
species <- c("Sambar Deer", "Bay Cat")

# create species subdirectories
SpeciesFolderCreate1 <- createSpeciesFolders (inDir               = wd_createSpeciesFoldersTest,
                                              species             = species,
                                              hasCameraFolders    = FALSE,
                                              removeFolders       = FALSE)
  
SpeciesFolderCreate1


# delete empty species directories
SpecFolderCreate2 <- createSpeciesFolders (inDir               = wd_createSpeciesFoldersTest,
                                           species             = species,
                                           hasCameraFolders    = FALSE,
                                           removeFolders       = TRUE)

SpecFolderCreate2

## -----------------------------------------------------------------------------
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")

# run check with 120 seconds (2 minutes) maximum time differnce
check.folders <- checkSpeciesIdentification(inDir               = wd_images_ID,
                                            IDfrom              = "directory",
                                            hasCameraFolders    = FALSE,
                                            maxDeltaTime        = 120)
check.folders

## -----------------------------------------------------------------------------
# check only station A and B (will give no results)
checkSpeciesIdentification(inDir               = wd_images_ID,
                           IDfrom              = "directory",
                           hasCameraFolders    = FALSE,
                           maxDeltaTime        = 120,
                           stationsToCheck     = c("StationA", "StationB"))


# Exclude chevrotains (Tragulus spp).  will give no results
checkSpeciesIdentification(inDir               = wd_images_ID,
                           IDfrom              = "directory",
                           hasCameraFolders    = FALSE,
                           maxDeltaTime        = 120,
                           excludeSpecies      = "TRA")

## -----------------------------------------------------------------------------

# copy sample images to another location (so we don't mess around in the package directory)
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")
file.copy(from = wd_images_ID, to = tempdir() , recursive = TRUE)
wd_images_species_copy <- file.path(tempdir(), "sample_images")


species_names_append <- appendSpeciesNames(inDir               = wd_images_species_copy,
                                           IDfrom              = "directory",
                                           hasCameraFolders    = FALSE
)

head(species_names_append)

species_names_remove <- appendSpeciesNames(inDir               = wd_images_species_copy,
                                           IDfrom              = "directory",
                                           hasCameraFolders    = FALSE,
                                           removeNames         = TRUE
)

head(species_names_remove)


## -----------------------------------------------------------------------------
# again, we use a temporary directory for demonstration. Change this in your own code!
wd_images_species_copy <- file.path(tempdir(), "sampleSpeciesImages")

species_to_copy <- "VTA"    # = Viverra tangalunga, Malay Civet

specImagecopy <- getSpeciesImages(species                 = species_to_copy,
                                  IDfrom                  = "directory",
                                  inDir                   = wd_images_ID,
                                  outDir                  = wd_images_species_copy,
                                  createStationSubfolders = FALSE
  )

specImagecopy

## ----eval = FALSE-------------------------------------------------------------
#  # all images in one directory, no station subdirectories
#  # column "station" in outtable will be uninformative
#  ID_check1 <- checkSpeciesIdentification(inDir    = "C:/Users/Peter/individualID",
#                                          stationsToCheck   = "SpeciesA",         # no station subdirectories
#                                          IDfrom            = "metadata",
#                                          hasCameraFolders  = FALSE,
#                                          metadataSpeciesTag          = "Example_Species_ID_Paul",
#                                          metadataSpeciesTagToCompare = "Example_Species_ID_Peter",
#                                          maxDeltaTime        = 60)
#  
#  # check results with either of the following two:
#  # ID_check1[[2]]
#  # ID_check1$IDconflictCheck
#  
#  # all images in station subdirectories
#  ID_check2 <- checkSpeciesIdentification(inDir    = "C:/Users/Peter/individualID/SpeciesA",  # with station subdirectories
#                                          IDfrom   = "metadata",
#                                          hasCameraFolders    = FALSE,
#                                          metadataSpeciesTag  = "Example_Species_ID_Paul",
#                                          metadataSpeciesTagToCompare = "Example_Species_ID_Peter",
#                                          maxDeltaTime        = 60)

