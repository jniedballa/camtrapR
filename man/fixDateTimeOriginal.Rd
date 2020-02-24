\name{fixDateTimeOriginal}
\alias{fixDateTimeOriginal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fix DateTimeOriginal Exif metadata tag in Reconyx Hyperfire cameras
}
\description{
Some camera models don't store the date/time information in the standard Exif metadata tag. Consequently, camtrapR cannot find that information. This function uses Exiftool to update the DateTimeOriginal metadata tag in all images within a directory to make them readable with camtrapR (and other software).

}
\usage{
  fixDateTimeOriginal(inDir,
      recursive = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{inDir}{
  character. Name of the directory containing images to be fixed
}
\item{recursive}{
  logical. Recursively find images in subdirectories of \code{inDir}?
}

}
\details{
Some Reconyx Hyperfire cameras (e.g. HC500) are known to show this problem.
}

\value{
Returns invisibly the messages returned by the Exiftool call (warnings etc.).
}

\author{
Juergen Niedballa
}

\section{Warning }{
Please make a backup of your images before running this function.}


\references{
This function uses the code from: \cr
Tobler, Mathias (2015). Camera Base Version 1.7 User Guide \url{http://www.atrium-biodiversity.org/tools/camerabase/files/CameraBaseDoc1.7.pdf}
}

\examples{
\dontrun{
# a hypothetical example

wd_images_hyperfire <- "C:/Some/Directory"

fixDateTimeOriginal(inDir     = wd_images_hyperfire,
                    recursive = TRUE)
}
}