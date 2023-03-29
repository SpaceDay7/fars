
#### Peer-graded Assignment: Documenting Code ####


#' This function reads an existing CSV file and converts it into a
#' data table in R by returning a tbl_df. The file must be imported from your
#' your directory. If not the function will produce an error.
#'
#' @param  filename  provides the name of the csv file
#'
#' @return a table data frame - tbl_df
#' An error will occur if the file does not exist
#'
#' @importFrom readr read_csv
#' Note: the progress is set to FALSE
#'
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' x <- fars_read("accident_2013.csv.bz2")
#' fars_read("accident_2013.csv.bz2")
#' }
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

#' This function creates the name for the data file to be downloaded. The function takes the input of the year and returns the filename.
#'
#' @param year This is the year of the data to be included in the file name.
#' This can be inputted as an integer, numeric, or characterised number - "2013".
#' An error will occur if an invalid year is entered.
#'
#' @return This function returns the full file name of the file "accident" and the for the year specified.
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' make_filename("2014")
#' }
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' This function reads one or more data files at once. It tries to read in a
#' file for each month and year and then append the month and year columns.
#' If there is no applicable year, the function returns NULL and continues to
#' the next year.
#'
#' @param years A vector or list containing the years of the files. Must provide the
#' years of data that are available.
#'
#' @inheritParams make_filename
#'
#' @inheritParams fars_read
#'
#' @importFrom dyplyr mutate
#'
#' @importFrom dyplyr select
#'
#' @return A list of dataframe(s). If year is not valid, returns NULL.
#'
#' @examples
#' \dontrun {
#' fars_read_years(c(2014,2015))
#' fars_read_years(2016)
#' }
#'
#' @export
#'

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



#' This function gives the summary of results per year as a list of years for
#' each data set imputed into it.
#'
#'  @param years A vector or list containing the years of the files. Must provide the
#'  years of data that are available.
#'
#'  @inheritParams fars_read_years
#'
#'  @importFrom dyplyr bind_rows, group_by, summarise
#'
#'  @importFrom tidyr spread
#'
#'  @return a dataframe containing the sample size by dataset summarising by year
#'  and month with years as columns and months as rows
#'
#'  @examples
#'  \dontrun{
#'  fars_summarize_years(c(2013,2014,2015))
#'  }
#'
#'  @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' This function plots a point at the state number where the accidents reported for each year
#' happened. The year and state must be provided in order to plot the accidents.
#' If no accidents were reported in a state for a given year,then there will be no points to plot.
#' If the number provided for the state is  not in the desired range then an error will be given.
#'
#'  @param state.num an integer indicating the id for the state to be plotted
#'
#'  @inheritParams make_filename
#'
#'  @return a map of the state and location plotting the points where the accidents were reported for the
#'  given state by year
#'
#'  @importFrom dyplyr filter
#'
#'  @importFrom maps map
#'
#'  @importFrom graphics points
#'
#'  @examples
#'  \dontrun{
#'  fars_map_state(01,2013)
#'  }
#'
#'  @export
#'

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

setwd("..")
getwd()
install("fars")
