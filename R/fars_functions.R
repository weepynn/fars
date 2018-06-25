


#' \code{fars_read} reads in data from a csv file. It takes in a character string of a path of a filename to read into R.
#' If the file does not exist, it throws an error messsage, "file \code{filename} does not exist".
#' If the file exists, it returns a data frame of data read in from the csv file.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @param filename a character string of the csv filename to be read
#' @return a data frame of the data from the csv file; an error message otherwise
#' @examples
#' \dontrun{
#' far_read("data/filename.csv")
#' }
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' \code{make_filename} takes in a specific 'year', creates and returns a filename specifying the year of the dataset. The filename includes the
#' the \code{bz2} extension for importing zipped files. For example, if the year specified is \code{2011}, the filename
#' will be \code{accident_2011.csv.bz2}.
#'
#' @param year A string or integer input specifying the year of the dataset.
#' @return A character string which is the filename.
#' @examples
#' \dontrun{
#' make_filename(2011)
#' }
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' \code{fars_read_years} takes in a vector of 'years' specified to create and return a list of dataframes
#' with only the columns 'YEAR' and 'MONTH'. If one of the name of the file created by \code{make_filename} function does not exist,
#' a warning message \code{invalid year: xxxx} will be returned.
#'
#' @param years vector of years to read in
#' @importFrom dplyr mutate select %>%
#' @return returns a list of dataframes with colums 'MONTH' and 'YEAR', NULL when an error is encountered.
#' @examples
#' \dontrun{
#' far_read_years(c(2011, 2012))
#' }
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

#' \code{fars_summarize_years} combines all the dataframes obtained from the \code{fars_read_years} function into one single dataframe,
#' groups the data into each specific 'year' and 'month', summarises by counting the number of data points for each group, and then transforms the
#' table from the long to wide format according to the 'year' and number of counts for each group variables.
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @param years vector of years to read in
#' @return a dataframe of summarised data of wide format
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2011, 2012))
#' }
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' \code{fars_map_state} takes in a specific state and specific year and plots accident points according to latitude and longtitude on a state map.
#' If the input state is invalid, displays error message, "invalid STATE number: ". If there is no accident in that state, displays message,
#' "no accident to plot".
#' @param state.num state number specified
#' @param year year specified
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom dplyr filter
#' @return a date.frame of filtered data for a particular state and year; if there are no rows, it returns invisible(NULL)
#' @examples
#' \dontrun{
#' fars_map_state(1,2011)
#' }
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
