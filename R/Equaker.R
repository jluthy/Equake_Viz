#' Functions to Analyze Data Set for earthquakes for Capstone Proj in Coursera.
#' Read in the tab delimited data file from NOAA.
#' Creates a data frame from raw data file.
#' @note readr read.delim will read the file and creates a data frame and must have an element in each column,
#' therefore added the \code{fill=TRUE} to fill in the blanks that are present in table with NA's
#' @param filename the name of the .tsv file
#' @return this funtion will return data frame from tab separated file and fill balnks with NA.
#' @examples \dontrun {
#' eq_readIn_data(filename="results.tsv")
#' }
#' @export 
eq_readIn_data <- function(filename) {
  data <- read.delim(filename, sep = "\t", fill = TRUE, header = TRUE)
}
# df <- eq_readIn_data("./data/results.tsv")
#' Cleaning of the data.
#' This will clean the data to remove colon from Location Names and convert it to title case for later use.
#' @note This function will clean up original LOCATION_NAME column data to remove colon and convert to title case for use later.
#' @param data is the data frame with data and NA from previous step
#' @return this function will return 'cleaned' data for the previously created data frame
#' @importFrom dplyr mutate
#' @importFrom purrr pmap_chr
#' @importFrom stringr str_trim str_to_title
#' @importFrom magrittr %>%
#' @examples \dontrun {
#' eq_clean_data(df)
#' }
#' @export
eq_clean_data <- function(data){
  df <- data %>%
    dplyr::mutate(
      LOCATION_NAME = purrr::map2_chr(COUNTRY, LOCATION_NAME,
                                      function(COUNTRY, LOCATION_NAME) {
                                        base::gsub(paste0(COUNTRY, ":"),
                                                   "",
                                                   LOCATION_NAME)
                                      }),
      # remove white spaces:
      LOCATION_NAME = stringr::str_trim(LOCATION_NAME),
      
      # required by the assignment:
      LOCATION_NAME = stringr::str_to_title(LOCATION_NAME)
    )
}
# df_clean <<- eq_clean_data(df)
#' Further cleaning of the data.
#' Filters the data by date and country.
#' @note can include more than one country name to display multiple timelines.
#' @param data the cleaned data to load into function
#' @return this function will return a timeline showing dots of relative size color relating to data
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom magrittr %>%
#' @examples  \dontrun {
#' geom_timeline(df_clean)
#' }
#' @export
geom_timeline <- function(data) {
  df_filtered <<- data %>%
    filter(COUNTRY %in% c("USA") & YEAR >= 1750)
  g <- ggplot2::ggplot(df_filtered, aes(x = YEAR, y = COUNTRY, size = EQ_PRIMARY, color = DEATHS, alpha = .3)) +
    geom_point()
  
  g
}
# geom_timeline(df_clean)
#' Timeline labels
#' Add text labels for each plotted dot on timeline
#' @param data the cleaned data
#' @return this will return a timeline as above but with labels for data points
#' @importFrom ggplot2 ggplot
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @examples  \dontrun {
#' geom_timeline_label(df_filtered)
#' }
#' @export
geom_timeline_label <- function(data) {
  df_filtered <<- data %>%
    filter(COUNTRY %in% c("USA") & YEAR >= 1981)
  g <- ggplot2::ggplot(df_filtered, aes(x = YEAR, y = COUNTRY, size = EQ_PRIMARY, color = DEATHS, alpha = .3)) +
    geom_point() +
    geom_text(label = df_filtered$LOCATION_NAME, angle = 45, size = 2, hjust = 0, nudge_y = .025, check_overlap = TRUE)
  g
}
# geom_timeline_label(df_clean)
#' Make a label for the popups
#' Furthering the detailed information in the annotation boxes
#' @return label information
#' @param data the cleaned and fileterd data frame
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom purrr pmap_chr
#' @export
eq_create_label <- function(data) {
  labeling <- function(loc, int, td, dt) {
    label <- ''
    if (!(is.na(dt))) {
      label <- paste0(label,
                      '<b>Date:</b> ',
                      as.Date(dt, origin = '1981-01-01'),
                      '<br/>')
    }
    if (!(is.na(loc))) {
      label <- paste0(label, '<b>LOCATION_NAME:</b> ', loc, '<br/>')
    }
    if (!(is.na(int))) {
      label <- paste0(label, '<b>INTENSITY:</b> ', int, '<br/>')
    }
    if (!(is.na(td))) {
      label <- paste0(label, '<b>TOTAL_DEATHS:</b> ', td, '<br/>')
    }
    label
  }
  
  data <- data %>%
    dplyr::mutate_(popup_text = ~purrr::pmap_chr(
      list(LOCATION_NAME, EQ_PRIMARY,
           TOTAL_DEATHS, YEAR),
      labeling
    ))
  data$popup_text 
  data
}
#' Plot the map
#' PLots the global map with annotations for earthquakes within filtered data set.
#' @return this will return a map with points plotted for records in the US.
#' @param data the cleaned and filtered data frame
#' @param annot_col the information used in popup label on map
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom leaflet leaflet addCircleMarkers
#' @examples \dontrun {
#' eq_map(df_filtered)
#' }
#' @export
eq_map <- function(data, annot_col = 'YEAR') {
  data <- data %>%
    dplyr::mutate_(popup_col = as.name(annot_col))
      m <- leaflet::leaflet(data = data) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      lng = ~ LONGITUDE,
      lat = ~ LATITUDE,
      radius = ~ EQ_PRIMARY,
      weight = 1,
      popup = ~ as.character(popup_col)
    )
  
  m
}
