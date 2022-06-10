#' Read fars data
#'
#' This function reads a fars data set from the working directory. It first checks
#' to make sure the specified file actually exists, then imports it with read_csv
#' if it does. An error is thrown if the file does not exist.
#'
#' @param filename The name of the file to be imported
#'
#' @return This function returns a tibble of the set specified.
#'
#' @examples
#' \dontrun{
#' fars_data <- readr::read_csv("accident_2013.csv.bz2")
#' }
#'
#' @importFrom readr read_csv
#' @import dplyr
#' @import tidyr
#' @import maps
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Build a fars file name
#'
#' This function builds a file name for a compressed fars data set using only the
#' year of the set. Note that no checks are run to ensure that a file with
#' the given name actually exists in the working directory.
#'
#' @param year The year of the data set. Must be an integer or coercible to one.
#'
#' @return This function returns a file path in the standard format of compressed fars files.
#'
#' @examples
#' \dontrun{
#' file_path <- make_filename(2014)
#' }
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read multiple fars data sets simultaneously
#'
#' This functions accepts a vector of years and returns a list of fars data frames.
#' A warning message is passed if any of the years specified do not correspond to
#' valid files in the workingn directory.
#'
#' @param years An integer vector of years.
#'
#' @return A list of tibbles, one per year specified. Entries in this list will
#' be null if corresponding values of the input do not give rise to valid file names.
#'
#' @examples
#' \dontrun{
#' years <- 2013:2015
#' fars_list <- fars_read_years(years)
#' }
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Import and summarize multiple fars data sets.
#'
#' This function imports multiple fars data sets, specified by a numeric vector of
#' years, and returns a tibble with monthly counts for those years.
#'
#' @param years A numeric vector of years.
#'
#' @return A tibble with 12 rows, one for each calendar month. In addition to a column
#' of months, the output includes one column for each year specified.
#'
#' @examples
#' \dontrun{
#' years <- 2013:2015
#' fars_summary <- fars_summarize_years(years)
#' }
#'
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot crash locations
#'
#' This function generates a simple plot of the locations of traffic accidents
#' for a specified state. Errors are thrown if an invalid year or state number is input,
#' and a warning is generated if there are no accidents for the year/state specified.
#'
#' @param state.num The state for which accident locations are to be plotted, specified as a number.
#' @param year The 4-digit year for which the data should be plotted
#'
#' @return A simple plot of accident locations.
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, 2014) # Alabama map from 2014
#' fars_map_state(18, 2013) # Indiana map from 2013
#' }
#'
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
