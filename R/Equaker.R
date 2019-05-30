#' 
#' Functions to Analyze Data Set on Earthquake Activity for Capstone Proj in Coursera.
#' Read in the tab delimited data file from NOAA.
#' Creates a data frame from raw data file.
#' data obtained from (National Geophysical Data Center / World Data Service (NGDC/WDS)
#'                           Significant Earthquake Database. National Geophysical Data Center, NOAA. 
#'                           doi10.7289/V5TD9V7K)
#' Cleaning the data by formatting the date time & Latitude & Longitude
#' @param df is the data frame with data and NA from previous step.
#' @return this function will return 'cleaned' data for the previously created data frame
#' @importFrom lubridate as_date year month day
#' @examples \dontrun{
#' df = readr::read_delim("./inst/extdata/results.tsv", delim = "\t") %>% eq_clean_data()
#' }
#' @export 
  eq_clean_data <- function(df){
    df$DATE = lubridate::as_date('01-01-1970',format = '%d-%m-%Y', tz = 'UTC')
    lubridate::year(df$DATE) = df$YEAR
    lubridate::month(df$DATE) = df$MONTH
    lubridate::day(df$DATE) = df$DAY
    df$LATITUDE = as.numeric(df$LATITUDE)
    df$LONGITUDE = as.numeric(df$LONGITUDE)
    return(df)
  }  
#' Clean Location name
#' 
#' This will clean the data to remove colon from Location Names and convert it to title case for later use.
#' 
#' @param df is the data frame from intial cleaning of the raw data
#' @importFrom stringr str_split_fixed str_to_title
#' @examples \dontrun{
#' df = readr::read_delim("./inst/extdata/results.tsv", delim = "\t") %>% eq_clean_data() %>% 
#' eq_location_clean()
#' }
#' @export
  eq_location_clean <- function(df){
    df$LOCATION_NAME <- stringr::str_split_fixed(df$LOCATION_NAME, ": ", 2) [,2]
    df$LOCATION_NAME <- stringr::str_to_title(df$LOCATION_NAME)
    return(df)
  }
#' Further cleaning of the data.
#' 
#' Filters the data by date and country and then plots a timeline
#' 
#' @note can include more than one country name to display multiple timelines.
#' @param df the cleaned data to load into function
#' @return this function will return a timeline showing dots of relative size color relating to data
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot geom_point
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @examples  \dontrun{
#' df = readr::read_delim("./inst/extdata/results.tsv", delim = "\t") %>% eq_clean_data() %>% 
#' eq_location_clean()
#' geom_timeline(df)
#' }
#' @export
geom_timeline <- function(df) {
  df2 <<- df %>%
    filter(.data$COUNTRY %in% c("USA")) %>%
    filter(.data$DATE >= "1987-10-01")
  
  g <- ggplot2::ggplot(df2, aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, color = DEATHS, alpha = .3)) +
    geom_point()
  g
}
#' Timeline labels
#' 
#' Add text labels for each plotted dot on timeline
#' @param df the cleaned data
#' @return this will return a timeline as above but with labels for data points
#' @importFrom ggplot2 ggplot geom_point geom_text
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @examples  \dontrun{
#' df = readr::read_delim("./inst/extdata/results.tsv", delim = "\t") %>% eq_clean_data() %>% 
#' eq_location_clean()
#' geom_timeline_label(df)
#' }
#' @export
geom_timeline_label <- function(df) {
  df3 <<- df %>%
    filter(.data$COUNTRY %in% c("USA")) %>%
    filter(.data$DATE >= "1987-10-01")

  g <- ggplot2::ggplot(df3, aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, color = DEATHS, alpha = .3)) +
    geom_point() +
    geom_text(label = df3$LOCATION_NAME, angle = 45, size = 2, hjust = 0, nudge_y = .025, check_overlap = TRUE)
  
  g
}
#' Make a label for the popups
#' 
#' Furthering the detailed information in the annotation boxes
#' @return label information
#' @return subdf an updated data frame with label information
#' @param data the cleaned and fileterd data frame
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom purrr pmap_chr
#' @examples \dontrun{
#' df = readr::read_delim("./inst/extdata/results.tsv", delim = "\t") %>% eq_clean_data() %>% 
#' eq_location_clean()
#' geom_timeline_label(df)
#' eq_create_label(df)}
#' @export
eq_create_label <- function(data) {
  labeling <- function(loc, int, td, dt) {
    label <- ''
    if (!(is.na(dt))) {
      label <- paste0(label,
                      '<b>DATE:</b> ',
                      as.Date(dt, origin = '1987-10-01'),
                      '<br/>')
    }
    if (!(is.na(loc))) {
      label <- paste0(label, '<b>LOCATION_NAME:</b> ', loc, '<br/>')
    }
    if (!(is.na(int))) {
      label <- paste0(label, '<b>EQ_PRIMARY:</b> ', int, '<br/>')
    }
    if (!(is.na(td))) {
      label <- paste0(label, '<b>TOTAL_DEATHS:</b> ', td, '<br/>')
    }
    label
  }
  df <- data %>%
    dplyr::mutate(
      popup_text = purrr::pmap_chr(list(LOCATION_NAME, EQ_PRIMARY,
                                        TOTAL_DEATHS, DATE), labeling))
  
  df$popup_text
  df
}
#' Plot the map
#' 
#' Plots the global map with annotations for earthquakes within filtered data set.
#' @return this will return a map with points plotted for records in the US.
#' @param data the cleaned and filtered data frame
#' @param annot_col the information used in popup label on map
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @examples \dontrun{
#' df = readr::read_delim("./inst/extdata/results.tsv", delim = "\t") %>% eq_clean_data() %>% 
#' eq_location_clean()
#' geom_timeline(df)
#' eq_create_label(df)
#' eq_map(df)}
#' @export
eq_map <- function(data, annot_col = 'popup_text') {
    m <- leaflet::leaflet(data = data) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      color = "#03F",
      lng = ~ LONGITUDE,
      lat = ~ LATITUDE,
      radius = data$INTENSITY,
      weight = 11,
      popup = ~ eval(parse(text = annot_col)))
  
  m
}

 # df <- readr::read_delim("./inst/extdata/results.tsv", delim = "\t") %>% eq_clean_data() %>%
 # eq_location_clean()
 # geom_timeline(df)
 # geom_timeline_label(df)
 # eq_create_label(df)
 # eq_map(df)

 