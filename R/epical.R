
#' Calculate Epidemiological Weeks from Calendar Dates
#'
#' Function to calculate the epidemiological week and year in which calendar
#' dates fall.
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
#' @param date A vector that can be coerced to a \code{Date}
#' @return A data frame with columns labeled "epi-week" and "epi-year". The
#' object also has class tbl_df and tbl for those who use the \code{dplyr}
#' package.
#'
#' Each item in the parameter vector corresponds to a row in the returned data
#' frame.
#'
#' @examples
#' epi_week("2015-01-01")
#' epi_week(seq(as.Date("2014-12-26"), as.Date("2015-01-12"), by = "day"))
#'
#' @family epi calendar functions
#'

epi_week <- function(date) {

  date <- as.Date(date)
  dplyr::bind_rows(
    lapply(date, function(x) {
      # wday returns the day of the week as a decimal number (1-7, Sunday is 1)
      saturday_on_or_after_x <- x + (7 - lubridate::wday(x))
      sunday_on_or_before_x <- x - (lubridate::wday(x) - 1)
      if (lubridate::yday(saturday_on_or_after_x) %in% 4:10) {
        # this is by definitation, because  the first epidemiological week of
        # the year ends on the first Saturday of January, provided that it falls
        # at least four days into the month. thus the Saturday ending the first
        # epi-week must fall on January 4th through 10th
        epi_week <- 1
        epi_year <- lubridate::year(saturday_on_or_after_x)
      } else {
        # in the case of the last epi-week of the year, modding the Sat. after
        # would yield an epi-week of 1 instead of 52/53, so instead we have to
        # mod the Sun. before
        epi_week <- ((lubridate::yday(sunday_on_or_before_x) + 2) %/% 7) + 1
        epi_year <- lubridate::year(sunday_on_or_before_x)
      }
      return(data.frame(epi_week, epi_year))
    })
  )
}

#' Calculate the Start Dates of Epidemiological Years
#'
#' Function to calculate the start dates of epidemiological years.
#'
#' Epidemiological weeks, also called "epi-weeks" or "epi weeks", are a
#' standardized method of assigning days to a week of the year. The first
#' epi-week always begins on the Sunday that falls in the range of December
#' 29 to January 4. This function determines which date the first day of
#' epi-week 1 falls in.
#'
#' I'm not sure if it can handle NAs. Some improvements may be needed.
#'
#' @param year An object that can be coerced to a \code{numeric}
#' @return A \code{Date} vector
#'
#' @examples
#' epi_year_start_date(c(2014,2015))
#'
#' @family epi calendar functions
#'

epi_year_start_date <- function(year) {
  start_date <- sapply(year, function(x) {
    possible_dates <- seq(as.Date(paste0(x - 1, "-12-29")),
                          as.Date(paste0(x, "-01-04")),
                          by = "day")
    possible_dates[min(which(epi_year(possible_dates) == x))]
  })
  class(start_date) <- "Date"  # sapply unlists and drops the class, so put it back
  return(start_date)
}

#' Calculate the Start Dates of Epidemiological Weeks
#'
#' Function to calculate the start dates of epidemiological weeks.
#'
#' Epidemiological weeks, also called "epi-weeks" or "epi weeks", are a
#' standardized method of assigning days to a week of the year. This function
#' determines the start date of a given set of epi-weeks identified by epi-week
#' number and year.
#'
#' \code{epi_week} and \code{epi_year} are recycled if necessary.
#'
#' @param epi_week An object that can be coerced to a \code{numeric}
#' @param epi_year An object that can be coerced to a \code{numeric}
#' @return A \code{Date} vector
#'
#' @examples
#' epi_week_start_date(1, 2010:2015)
#' epi_week_start_date(1:52, 2015)
#' epi_week_start_date(c(-1, 0, TRUE, 52, 53, 999, "a"), 2015) # NAs
#'
#' @family epi calendar functions
#'

epi_week_start_date <- function(epi_week, epi_year) {
  epi_week <- as.numeric(epi_week)
  epi_year <- as.numeric(epi_year)
  z <- mapply(function(x, y) {
    epi_year_start_date(y) + ((x -1) * 7)
  }, epi_week, epi_year)
  class(z) <- "Date"  # sapply unlists and drops the class, so put it back
  z[epi_week(z) != epi_week & epi_year(z) != epi_year] <- NA
  if (any(is.na(z))) {
    warning("Some start dates could not be calculated: NAs introduced",
            call. = FALSE, noBreaks. = TRUE)
  }
  return(z)
}
