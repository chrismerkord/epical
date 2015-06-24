#' Convert between dates and epidemiological weeks/years.
#'
#' epical allows you to calculate back and forth between epi-weeks and
#' calendar dates.
#'
#' To determine which epi-week a calendar date falls in, use
#' \code{\link{epi_week}}.
#'
#' To add columns for epi-week and epi-year to a data frame with a date column,
#' use \code{\link{add_epi_week}}.
#'
#' To determine the start date of an epi-week or epi-year, use
#' \code{\link{epi_week_date}} or \code{\link{epi_year_start}}.
#'
#' @docType package
#' @name epical

NULL

#' Date Conversion to Epidemiological Weeks
#'
#' Function to calculate the epidemiological weeks in which calendar dates fall.
#'
#' Epidemiological weeks, also called "epi-weeks" or "epi weeks", are a
#' standardized method of assigning days to a week of the year. Epi-weeks are
#' used by the WHO, CDC, and many other health organizations.
#'
#' Here we define an epi-week as starting on a Sunday and ending on a Saturday,
#' with the first epi-week of the year ending on the first Saturday of the year
#' that falls at least four days into the year. In other words, the first
#' epi-week always begins on the Sunday that falls in the range of December 29
#' to January 4.
#'
#' Because some dates in epi-week 1 can fall within the previous
#' calendar year, and dates in epi-week 52/53 can fall within the next calendar
#' year, it is necessary to specify both the epi-week number and an
#' "epi-year" so that dates can be grouped correctly.
#'
#' The function first converts \code{date} to a \code{Date} object using the
#' function \code{as.Date()}. I welcome suggestions for making this more robust.
#'
#' @param x A Date vector, or another vector that can be converted to Date using
#' \code{as.Date}
#' @param ... Additional parameters passed to \code{as.Date}
#' @return A data frame with columns labeled \code{epi_week} and
#' \code{epi_year}. The object also has class tbl_df and tbl for use with the
#' \code{dplyr} package.
#'
#' @examples
#' epi_week("2015-01-01")
#' epi_week("2015-150", format = "%Y-%j")
#' epi_week(seq(as.Date("2014-12-26"), as.Date("2015-01-12"), by = "day"))
#'
#' @family epi calendar functions
#'

epi_week <- function(x, ...) {

  require(dplyr)
  require(lubridate)

  x <- as.Date(x, ...) # convert x to calendar dates

  # to make the function faster, reduce input to distinct dates befire
  # calculating epi-weeks and epi-years
  df2 <- data_frame(x) %>%
    distinct() %>%
    mutate(sat_after = x + (7 - lubridate::wday(x)),
           sun_before = x - (lubridate::wday(x) - 1),
           epi_week = NA,
           epi_year = NA)

  # this is by definitation, because  the first epidemiological week of
  # the year ends on the first Saturday of January, provided that it falls
  # at least four days into the month. thus the Saturday ending the first
  # epi-week must fall on January 4th through 10th
  case1 <- yday(df2$sat_after) %in% 4:10
  df2[case1, "epi_week"] <- 1
  df2[case1, "epi_year"] <- lubridate::year(df2$sat_after[case1])

  # in the case of the last epi-week of the year, modding the Sat. after
  # would yield an epi-week of 1 instead of 52/53, so instead we have to
  # mod the Sun. before
  case2 <- !(yday(df2$sat_after) %in% 4:10)
  df2[case2, "epi_week"] <- ((lubridate::yday(df2$sun_before[case2])
                              + 2) %/% 7) + 1
  df2[case2, "epi_year"] <- lubridate::year(df2$sun_before[case2])

  # merge the data frame of distinct dates back into the original
  suppressMessages(
    data_frame(x) %>%
    left_join(df2) %>%
    select(epi_week, epi_year) # function returns these two columns
  )
}

#' Add Epi-weeks and Epi-years to a Data Frame
#'
#' add_epi_week takes a data frame that already contains a date column and adds
#' columns for epi-week and epi-year.
#'
#' If you do not provide the name of the date column in the supplied data frame,
#' the function looks for a column named "date". If it doesn't find one, it
#' stops with an error.
#'#'
#' @param data A data frame
#' @param date_col Name of date column
#' @param ... Additional parameters passed to \code{as.Date}
#' @return A data frame
#'
#' @examples
#' add_epi_week(data.frame(mydate = seq(as.Date("2014-12-26"),
#'                                  as.Date("2015-01-12"),
#'                                  by = "day")),
#'              "mydate")
#'
#' @family epi calendar functions
#'

add_epi_week <- function(data, date_col, ...) {
  if (!date_col %in% names(data)) stop(date_col, "not found in data frame")
  data <-as.data.frame(data)
  date_vector <- data[, date_col] # change from df to vector
  dplyr::bind_cols(data, epi_week(date_vector, ...)) %>% dplyr::tbl_df() # return merged df
  # bind_cols was supposed to return a tbl_df but for some reason it didn't, so
  # we had to change the class manually
}

#' #' Start Dates of Epidemiological Years
#'
#' Function to calculate the start dates of epidemiological years.
#'
#' The first epi-week always begins on the Sunday that falls in the range of
#' December 29 to January 4. This function determines which date of the first
#' day of epi-week 1. Note that the year of the returned date may not be the
#' same as the year provided, in cases where epi-week 1 begins in December of
#' the previous year.
#'
#' I'm not sure if it can handle NAs. Some improvements may be needed.
#'
#' @param year An object that can be coerced to a \code{numeric}
#' @return A \code{Date} vector
#'
#' @examples
#' epi_year_start(c(2014,2015))
#'
#' @family epi calendar functions
#'

epi_year_start <- function(year) {
  start_date <- sapply(year, function(x) {
    possible_dates <- seq(as.Date(paste0(x - 1, "-12-29")),
                          as.Date(paste0(x, "-01-04")),
                          by = "day")
    possible_dates[min(which(epi_week(possible_dates)$epi_year == x))]
  })
  class(start_date) <- "Date"  # sapply unlists and drops the class, so put it back
  return(start_date)
}

#' Date Conversion from Epidemiological Weeks
#'
#' Function to calculate dates from epidemiological weeks.
#'
#' This function determines the date of the first day of each epi-week,
#' optionally adding an offset to calculate the date of the last day of the
#' week, or any other arbitrary amount.
#'
#' \code{epi_week} and \code{epi_year} are recycled if necessary.
#'
#' @param epi_week An object that can be coerced to a \code{numeric}
#' @param epi_year An object that can be coerced to a \code{numeric}
#' @param offset A number of days to offset the returned dates by. The default
#' is 0 in which case the function returns the date of the first day of each
#' epi week. An offset of 6 would instead return the date of the last day of
#' each epi week.
#' @return A \code{Date} vector
#'
#' @examples
#' epi_week_date(1, 2010:2015)
#' epi_week_date(1:52, 2015)
#' epi_week_date(1:52, 2015, offset = 6) # return epi week end dates
#' epi_week_date(c(-1, 0, TRUE, 52, 53, 999, "a"), 2015) # NAs
#'
#' @family epi calendar functions
#'

epi_week_date <- function(epi_week, epi_year, offset = 0) {
  df_full <- dplyr::data_frame(epi_week, epi_year)
  df_distinct <- df_full %>%
    dplyr::distinct() %>%
    dplyr::mutate(epi_week = as.numeric(epi_week),
                  epi_year = as.numeric(epi_year))
  epi_week <- as.numeric(epi_week)
  epi_year <- as.numeric(epi_year)
  z <- mapply(function(x, y) {
    epi_year_start(y) + ((x -1) * 7)
  }, df_distinct$epi_week, df_distinct$epi_year)
  class(z) <- "Date"  # sapply unlists and drops the class, so put it back
  z[epi_week(z)$epi_week != df_distinct$epi_week &
      epi_week(z)$epi_year != df_distinct$epi_year] <- NA
  if (any(is.na(z))) {
    warning("Some start dates could not be calculated: NAs introduced",
            call. = FALSE, noBreaks. = TRUE)
  }
  df_distinct <- df_distinct %>% mutate(epi_date = z + offset)
  df_full <- suppressMessages(
    df_full %>% left_join(df_distinct)
  )
  return(df_full$epi_date)
}