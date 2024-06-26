% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadCsvFiles.R
\name{loadCsvFiles}
\alias{loadCsvFiles}
\title{Load CSV Files}
\usage{
loadCsvFiles(folder_path)
}
\arguments{
\item{folder_path}{A string. The path to the folder containing the CSV files.}
}
\value{
None. This function does not return a value; it assigns variables in the global environment.
}
\description{
This function loads all CSV files from a specified folder into the global environment.
Each CSV file is read into a data frame, its column names are cleaned, and it is
assigned to a variable named after the file (without the .csv extension).
}
\examples{
\dontrun{
loadCsvFiles("path/to/your/csvfiles")
}

}
\keyword{internal}
