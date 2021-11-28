#' Write a copyright tag into JPEG image metadata
#' 
#' This function writes a copyright tag into the copyright field of JPEG image
#' Exif metadata. It does so recursively, so it works both for images that are
#' sorted into subdirectories and unsorted images. Note that all images in
#' subdirectories of inDir will be tagged. It is not required to run this
#' function in the camtrapR workflow, but may be desired for data sharing or
#' publishing.
#' 
#' If askFirst = TRUE, the function will show a menu and asks the user to
#' confirm the action before execution. Type "1" to write copyright tags and
#' "2" to abort.
#' 
#' By default Exiftool creates a copy of each JPG image and preserves the
#' original images (without the copyright tag) as .JPG_original files. Note
#' that this behaviour will instantly double the number of images in inDir and
#' the disk space required. If this is not desired, set keepJPG_original =
#' FALSE.
#' 
#' \code{ignoreMinorErrors} is useful if copyright tags can't be updated
#' correctly. This can be caused by bad MakerNotes and so far was only observed
#' in Panthera V4 cameras. In that case, set \code{ignoreMinorErrors} to
#' \code{TRUE}. This will add the "-m" option to the Exiftool call, thereby
#' ignoring minor errors and warnings and assigning the copyright tag
#' regardless.
#' 
#' @param inDir character. Name of the directory containing camera trap images.
#' @param copyrightTag character. The tag to be written into the Exif Copyright
#' field
#' @param askFirst logical. Ask user to confirm before execution?
#' @param keepJPG_original logical. Keep original JPG files as .JPG_original
#' files (TRUE) or overwrite JPGs (FALSE)?
#' @param ignoreMinorErrors logical. Ignore minor errors that would cause the
#' function to fail (set TRUE for images with bad MakerNotes, observed in
#' Panthera V4 cameras)
#' 
#' @return An invisible list of Exiftool output.
#' 
#' More importantly, the specified copyright tag is written into the Copyright
#' field of the Exif metadata of all images in inDir.
#' 
#' @author Juergen Niedballa
#' 
#' @examples
#' 
#' 
#' \dontrun{
#' 
#' if (Sys.which("exiftool") != ""){        # only run this example if ExifTool is available
#' 
#' # copy sample images to temporary directory (so we don't mess around in the package directory)
#' wd_images_ID <- system.file(file.path("pictures", "sample_images_species_dir"), 
#'                             package = "camtrapR")
#' file.copy(from = wd_images_ID, to = tempdir(), recursive = TRUE)
#' wd_images_ID_copy <- file.path(tempdir(), "sample_images_species_dir")
#' 
#' # define a sample tag
#' copyrightTagToAdd <- "Your Name (Your Organisation)"
#' 
#' # add the tag to the images
#' addCopyrightTag(inDir        = wd_images_ID_copy, 
#'                 copyrightTag = copyrightTagToAdd)
#' 1     # we choose "YES", i.e., we want to add a copyright tag
#'                 
#' # you can check the outcome with function exifTagNames
#' 
#' metadat <- exifTagNames(wd_images_ID_copy)
#' metadat [metadat$tag_name == "Copyright",]
#' }
#' 
#' 
#' }
#' 
#' @export addCopyrightTag
#' 
addCopyrightTag <- function(inDir,
                            copyrightTag,
                            askFirst = TRUE,
                            keepJPG_original = TRUE,
                            ignoreMinorErrors = FALSE){

  stopifnot(is.character(inDir))
  stopifnot(file.exists(inDir))

  stopifnot(is.character(copyrightTag))
  stopifnot(length(copyrightTag) == 1)

  stopifnot(is.logical(keepJPG_original))

  if(isFALSE(keepJPG_original)) {
    overwrite_command <- " -overwrite_original "
  } else {overwrite_command <- " "}

  if(Sys.which("exiftool") == "") stop("cannot find ExifTool", call. = FALSE)

  if(isTRUE(askFirst)){
    message(paste("You are about to add the copyright tag '", copyrightTag, "' to all images in ", inDir, ". Are you sure?", sep = ""))
    switch(menu(c("YES", "NO")),
           doit <- TRUE,
           doit <- FALSE)
  } else {
    doit <- TRUE
  }

  if(isTRUE(doit)){

    command.tmp  <- paste0('exiftool -r ', ifelse(ignoreMinorErrors, "-m", ""), overwrite_command  ,'-Copyright="', copyrightTag, '" "', inDir, '"')

    output <- system(command.tmp, show.output.on.console = FALSE, intern = TRUE)
    message(paste(output[length(output) - 1],
                  output[length(output)]))
    if(grepl("files weren't updated due to errors",  output[length(output)])) {
      warning("There were problems adding the copyright tag. Try setting ignoreMinorErrors = TRUE", call. = FALSE)
    }
    return(invisible(output))
  } else {
    message("Did not write copyright tag")
    return(invisible(NULL))
  }
}
