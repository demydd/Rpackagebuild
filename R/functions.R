#' Read files
#'
#' @description The function read data from files specified by the user
#'
#' @param filename the file name of the data source is the input character vector. I could be defined by the user of other application.
#'                 If the file does not exist the function is terminated and the proper message is generated.
#' @return the function return the dataframe of 'tbl_df' class.
#'
#' @examples
#' input_data <- fars_read("data_source.txt")
#'
#' @export
#'
#' @importFrom ("readr", "read_csv")
#' @importFrom ("dplyr", "tbl_df")

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
########################################################################

#' Make the file name of the predefined template
#'
#' @description The function is designed to compile the file name of the perdifined template. The only variable (input) parameter is YEAR
#'
#' @param year the year of the accident data to be saved into archive file. It could be as integer or character vector.
#'             If year variable is of the wrong format so the function is stopped and R internal message is generated.
#'
#' @return the function return a character vector containing the file name required.
#'
#' @examples
#' filename <- make_filename('1978')
#'
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#####################################################################
#' Read data for sevelar years
#'
#' @description The function is designed to read data for the set of years.
#'
#' @param years the set of years (integer or character vector of the proper format) we are expecting to read data.
#'              IF the set of years containg the wrong year so the function execution will be stopped only for this year and the error message will be compiled for this year.
#'
#' @return the function returns the output of lapply funtion (it is a list). Each item of the list is the separate year dataset.
#'
#' @examples
#' years_output <- fars_read_years(c(1978, 197))
#'
#' @export
#'
#' @importFrom ("dplyr", "mutate", "select")

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

########################################################################3
#' Calculate the aggregation of record q-ty per annum
#'
#' @description The function aggregates the quantity of records per year.
#'
#' @param years the set of years (integer or character vector of the proper format) we are expecting to read data.
#'              IF the set of years containg the wrong year so the function execution will be stopped only for this year and the error message will be compiled for this year.
#'
#' @return the function returns the dataframe of year and quantity.
#'
#' @examples
#' years_output <- fars_summarize_years(c(1978, 1979))
#'
#' @export
#'
#' @importFrom("dplyr", "group_by", "summarize", "bind_rows")
#' @importFrom("tidyr", "map")


fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

####################################################################3
#' Build the map chart to visualize the annual results per a state.
#'
#' @description The function is reading raw data, manipulating them and develop the visualization via a map chart.
#'
#' @param year the year of the accident data to be read from archive file. It could be as integer or character vector.
#'             If year variable is of the wrong format so the function is stopped and R internal message is generated.
#' @param state.num the number of the state to read archived data (integer of character).  It could be as integer or character vector. If state.num variable is of the wrong format or value so the function is stopped and error message is generated.
#'
#'
#'
#' @return the graphic object (image).
#'
#' @examples
#' years_output <- fars_map_state(21,1978)
#'
#' @export
#'
#' @importFrom ("dplyr", "filter")
#' @importFrom ("maps", "spread")
#' @importFrom ("graphics", "points")

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
