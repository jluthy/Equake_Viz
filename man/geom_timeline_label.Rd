% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Equaker.R
\name{geom_timeline_label}
\alias{geom_timeline_label}
\title{Timeline labels}
\usage{
geom_timeline_label(df)
}
\arguments{
\item{df}{the cleaned data}
}
\value{
this will return a timeline as above but with labels for data points
}
\description{
Add text labels for each plotted dot on timeline
}
\examples{
 \dontrun{
df = readr::read_delim("./inst/extdata/results.tsv", delim = "\\t") \%>\% eq_clean_data() \%>\% 
eq_location_clean()
geom_timeline_label(df)
}
}
