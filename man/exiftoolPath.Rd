\name{exiftoolPath}
\alias{exiftoolPath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add a directory to PATH temporarily
}
\description{
Temporarily adds a directory to the environmental variable PATH for system calls from within R. This allows Windows users to store exiftool.exe anywhere on their hard drive. It is not needed on Linux or MacOS machines.
}
\usage{
exiftoolPath(exiftoolDir)
}

\arguments{
  \item{exiftoolDir}{
character. the directory in the file system containing exiftool.exe.
}

}
\details{
Several functions within this package depend on ExifTool. Under Windows, exiftool.exe cannot be used if it is not in a directory path specified in PATH. This can be solved by adding the directory containing exiftool.exe for temporary use within the running R process.
}

\value{
  invisible logical indicating whether \code{exiftoolDir} was added to PATH successfully (in the running R process).
}
% \references{
% ~put references to the literature/web site here ~
% }
\author{
Juergen Niedballa
}
\note{
The directories in PATH can be queried by \code{Sys.getenv("PATH")}.
 }

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
exiftool_dir <- "C:/Path/To/Exiftool"
exiftoolPath(exiftoolDir = exiftool_dir)

# check if it has been added to PATH
grepl(exiftool_dir,  Sys.getenv("PATH"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
