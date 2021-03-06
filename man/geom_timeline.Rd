% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Equaker.R
\name{geom_timeline}
\alias{geom_timeline}
\title{Further cleaning of the data.}
\usage{
geom_timeline(df)
}
\arguments{
\item{df}{the cleaned data to load into function}
}
\value{
this function will return a timeline showing dots of relative size color relating to data
}
\description{
Filters the data by date and country and then plots a timeline
}
\note{
can include more than one country name to display multiple timelines.
}
\examples{
 \dontrun{
df = readr::read_delim("./inst/extdata/results.tsv", delim = "\\t") \%>\% eq_clean_data() \%>\% 
eq_location_clean()
geom_timeline(df)
}
}
