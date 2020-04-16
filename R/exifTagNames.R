exifTagNames <- function(inDir, 
                         whichSubDir = 1, 
                         fileName,
                         returnMetadata = "DEPRECATED", 
                         returnTagGroup = "DEPRECATED"){
  
  
  if(returnMetadata != "DEPRECATED") message("Argument returnMetadata is ignored since version 2.0.0")
  if(returnTagGroup != "DEPRECATED") message("Argument returnTagGroup is ignored since version 2.0.0")
  
  if(Sys.which("exiftool") == "") stop("cannot find ExifTool", call. = FALSE)
  
  if(hasArg(fileName)){
    
    if(!hasArg(inDir)){
      file.tmp <- fileName
    } else {
      if(!dir.exists(inDir)) stop("Could not find inDir:\n", inDir, call. = FALSE)
      file.tmp <- file.path(inDir, fileName)
    }
    if(!file.exists(file.tmp)) stop(paste(file.tmp, "not found"))
    
  } else { # if fileName is not defined
    
    if(!hasArg(inDir))     stop("fileName is not defined. Please define 'inDir'", call. = FALSE)
    if(!dir.exists(inDir)) stop("Could not find inDir:\n", inDir, call. = FALSE)
    
    if(is.numeric(whichSubDir)){
      dir.tmp <- list.dirs(inDir, recursive = FALSE, full.names = TRUE)[whichSubDir]
    }
    if(is.character(whichSubDir)){
      dir.tmp <- file.path(inDir, whichSubDir)
    }
    if(!dir.exists(dir.tmp)) stop(paste("the specified subdirectory does not exist. Check argument 'whichSubDir.'\n", 
                                        dir.tmp), call. = FALSE)
    
    # filename of first image in specified directory
    file.tmp <- list.files(dir.tmp,
                           full.names = TRUE,
                           pattern    = ".JPG$|.jpg$",
                           recursive  = TRUE)[1]
    if(length(file.tmp) == 0) stop(paste("found no jpg in ", dir.tmp, sep = "\n"))
    
  }
  
  message(paste("Metadata of:", file.tmp, sep = "\n"))

  # generate exiftool calls
  
  # exiftool arguments: 
  # -s1: print tag  names instead of description
  # -t:  tab-delimited output
  # -G: return Group names # 0 (general location), 1 (specific location), 2 (category), 3 (document number), 4 (instance number), 5 (metadata path) or 6 (EXIF/TIFF format)
  
  groupHeadings_general  <- "-G0"    # can possibly also be -G0:1 
  #groupHeadings_specific <- "-G1"   # when using this separately from -G0, output is sorted differently (by group)
  
  
  command.tmp1  <- paste('exiftool -s1 -t ', groupHeadings_general, ' "', file.tmp, '"', sep = "")
  # run without -s1 option to extract tag descriptions instead of tag names
  command.tmp2 <- paste('exiftool -t ', groupHeadings_general, ' "', file.tmp, '"', sep = "")
  
  # run exiftool
  metadata.tmp1 <- system(command.tmp1, intern=TRUE)
  metadata.tmp2 <- system(command.tmp2, intern=TRUE)

  # convert output to data frames
  out1 <- read.table(text = metadata.tmp1, header = FALSE, 
                     col.names = c("tag_group", "tag_name", "value"), sep = "\t",
                     stringsAsFactors = FALSE, fill = TRUE)
  out2 <- read.table(text = metadata.tmp2, header = FALSE, 
                     col.names = c("tag_group", "tag_description", "value"), sep = "\t",
                     stringsAsFactors = FALSE, fill = TRUE)

  # make sure out1 and out2 are ordered identically
  if(any(out1$value != out2$value))  stop("Order of exiftool-extracted metadata differs. Please report this bug.")
  
  # combine data frames
  out <- cbind(tag_group       = out1$tag_group,
               tag_name        = out1$tag_name,
               tag_description = out2$tag_description,
               value           = out1$value)
  return(as.data.frame(out))
}
