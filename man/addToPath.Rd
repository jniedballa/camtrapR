% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addToPath.R
\name{addToPath}
\alias{addToPath}
\title{Add a directory to PATH temporarily}
\usage{
addToPath(directory)
}
\arguments{
\item{directory}{character. the directory in the file system to add to PATH 
(e.g. the directory containing exiftool.exe).}
}
\value{
invisible logical indicating whether \code{directory} was added to
PATH successfully (in the running R process).
}
\description{
Temporarily adds a directory to the environmental variable PATH for system
calls from within R. This allows Windows users to store exiftool.exe
anywhere on their hard drive and is useful if they cannot store the file 
in system directories. It is not needed on Linux or MacOS machines.
}
\details{
Several functions within this package depend on ExifTool. Under Windows,
exiftool.exe cannot be used if it is not in a directory path specified in
PATH. This can be solved by adding the directory containing exiftool.exe for
temporary use within the running R process. It can also be useful in other 
contexts besides Exiftool.
}
\note{
The directories in PATH can be queried by \code{Sys.getenv("PATH")}.
}
\examples{

exiftool_dir <- "C:/Path/To/Exiftool"
addToPath(directory = exiftool_dir)

# check if it has been added to PATH
grepl(exiftool_dir,  Sys.getenv("PATH"))

}
\author{
Juergen Niedballa
}
