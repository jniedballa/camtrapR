## ------------------------------------------------------------------------
library(camtrapR)
data(camtraps)

## ------------------------------------------------------------------------

# find data for 2 species with correctly spelled names
checkNames1 <- checkSpeciesNames (speciesNames = c("Bearded Pig", "Malayan Civet"),
                                  searchtype   = "common")
checkNames1

# and with scientific names (note that this is for a subspecies)
checkNames2 <- checkSpeciesNames (speciesNames = "Viverra tangalunga tangalunga",
                                  searchtype   = "scientific")
checkNames2

# an invalid name: the accepted name of the leopard cat is Prionailurus bengalensis
checkNames3 <- checkSpeciesNames (speciesNames = "Felis bengalensis",
                                  searchtype   = "scientific",
                                  accepted     = FALSE)
checkNames3

## an ambiguous name name: Chevrotain (Tragulus)
## this does not work in vignettes, but should run on your machine
 # checkNames4 <- checkSpeciesNames (speciesNames = c("Chevrotain"),
 #                                   searchtype   = "common")
 # 1              # making a choice from the menu
 # checkNames4

## ------------------------------------------------------------------------

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

## ------------------------------------------------------------------------
wd_images_ID <- system.file("pictures/sample_images", package = "camtrapR")

# run check with 120 seconds (2 minutes) maximum time differnce
check.folders <- checkSpeciesIdentification(inDir               = wd_images_ID,
                                            IDfrom              = "directory",
                                            hasCameraFolders    = FALSE,
                                            maxDeltaTime        = 120)
check.folders

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------

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


## ------------------------------------------------------------------------
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

## ----eval = FALSE--------------------------------------------------------
#  # all images in one directory, no station subdirectories
#  # column "station" in outtable will be uninformative
#  ID_check1 <- checkSpeciesIdentification(inDir                       = "C:/Users/Peter/individualID",
#                                          stationsToCheck             = "SpeciesA",         # no station subdirectories
#                                          IDfrom                      = "metadata",
#                                          hasCameraFolders            = FALSE,
#                                          metadataSpeciesTag          = "Example_Species_ID_Paul",
#                                          metadataSpeciesTagToCompare = "Example_Species_ID_Peter",
#                                          maxDeltaTime                = 60)
#  
#  # check results with either of the following two:
#  # ID_check1[[2]]
#  # ID_check1$IDconflictCheck
#  
#  # all images in station subdirectories
#  ID_check2 <- checkSpeciesIdentification(inDir                       = "C:/Users/Peter/individualID/SpeciesA",  # with station subdirectories
#                                          IDfrom                      = "metadata",
#                                          hasCameraFolders            = FALSE,
#                                          metadataSpeciesTag          = "Example_Species_ID_Paul",
#                                          metadataSpeciesTagToCompare = "Example_Species_ID_Peter",
#                                          maxDeltaTime                = 60)

