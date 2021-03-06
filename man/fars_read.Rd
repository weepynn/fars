% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{\code{fars_read} reads in data from a csv file. It takes in a character string of a path of a filename to read into R.
If the file does not exist, it throws an error messsage, "file \code{filename} does not exist".
If the file exists, it returns a data frame of data read in from the csv file.}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{a character string of the csv filename to be read}
}
\value{
a data frame of the data from the csv file; an error message otherwise
}
\description{
\code{fars_read} reads in data from a csv file. It takes in a character string of a path of a filename to read into R.
If the file does not exist, it throws an error messsage, "file \code{filename} does not exist".
If the file exists, it returns a data frame of data read in from the csv file.
}
\examples{
\dontrun{
far_read("data/filename.csv")
}
}
