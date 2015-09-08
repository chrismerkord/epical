#' Convert between dates and epidemiological weeks/years.
#'
#' epical allows you to calculate back and forth between epidemiological weeks
#' and calendar dates.
#'
#' Epidemiological weeks, also called "epi-weeks", "epi weeks", or "MMWR weeks"
#' are a standardized method of assigning days to a week of the year.
#' Epidemiological weeks are used by the WHO, CDC, and many other health
#' organizations.
#'
#' The WHO defines epidemiological weeks as starting on a Monday and ending on a
#' Sunday. The CDC defines epidemiological weeks as starting on a Sunday and
#' ending on a Saturday. In either case, the end of the first epidemiological
#' week of the year by definition must fall at least four days into the year. In
#' other words, the first epidemiological week always begins on a date between
#' December 29 and January 4.
#'
#' Every date can be assigned to a 7-day-long epidemiological week. Most years
#' have 52 epidemiological weeks, but some have 53.
#'
#' Because some dates in epidemiological week 1 can fall within the previous
#' calendar year, and dates in epidemiological week 52/53 can fall within the
#' subsequent calendar year, it is necessary to specify both the epidemiological
#' week number and an "epidemiological year" so that dates can be grouped
#' correctly by year.
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
#' Function to calculate the epidemiological weeks in which one or more calendar
#' dates fall.
#'
#' The function first converts \code{date} to a \code{Date} object using the
#' function \code{as.Date()}. I welcome suggestions for making this more robust.
#'
#' @param x A Date vector, or another vector that can be converted to Date using
#' \code{as.Date}
#' @param system Either "who" or "cdc". WHO epidemiological weeks start on
#' Monday. CDC epidemiological weeks (MMWR weeks) start on Sunday
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

epi_week <- function(x, system = "who", ...) {

  require(dplyr)
  require(lubridate)

  x <- as.Date(x, ...) # convert x to calendar dates

  # figure out how many days to shift the start of the week by
  if(system == "who") shift <- 1 else {
    if(system == "cdc") shift <- 0  else {
      stop("Argument 'system' must be one of c(\"who\", \"cdc\")")
    }
  }

  # to make the function faster, reduce input to distinct dates before
  # calculating epi-weeks and epi-years
  df2 <-
    data_frame(x) %>%
    distinct() %>%
    mutate(
      wday = wday(x - shift),                     # WHO: Mon.==1; CDC: Sun.==1
      week_start_date = x - (wday - 1),           # start of week containing x
      week_end_date = week_start_date + 6,        # end of week containing x
      week_start_yday = yday(week_end_date),
      week_end_yday = yday(week_end_date),
      # because start date could fall in prev year, use end date to identify epi
      # week 1. use start date to identify all other epi weeks
      epi_week = ifelse(
        week_end_yday %in% 4:10, 1,
        ((week_start_yday + 2) %/% 7) + 1
      ),
      epi_year = ifelse(
        week_end_yday %in% 4:10,
        year(week_end_date),
        year(week_start_date)
      )
    )

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
#' @param system Either "who" or "cdc". Defauly is "who".
#' See \code{\link{epi_week}} for details.
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

add_epi_week <- function(data, date_col, system = "who", ...) {
  if (!date_col %in% names(data)) stop(date_col, "not found in data frame")
  data <-as.data.frame(data)
  date_vector <- data[, date_col] # change from df to vector
  dplyr::bind_cols(data, epi_week(date_vector, system, ...)) %>%
    dplyr::tbl_df() # return merged df
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
#' @param system Either "who" or "cdc". Defauly is "who".
#' See \code{\link{epi_week}} for details.
#' @return A \code{Date} vector
#'
#' @examples
#' epi_year_start(c(2014,2015))
#'
#' @family epi calendar functions
#'

epi_year_start <- function(year, system = "who") {
  start_date <- sapply(year, function(x) {
    possible_dates <- seq(as.Date(paste0(x - 1, "-12-29")),
                          as.Date(paste0(x, "-01-04")),
                          by = "day")
    possible_dates[min(which(epi_week(possible_dates, system)$epi_year == x))]
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
#' @param system Either "who" or "cdc". Defauly is "who".
#' See \code{\link{epi_week}} for details.
#' @return A \code{Date} vector
#'
#' @examples
#' epi_week_date(1, 2010:2015)
#' epi_week_date(1:52, 2015)
#' epi_week_date(1:52, 2015, offset = 6) # return epi week end dates
#' epi_week_date(c(-1, 0, TRUE, 52, 53, 999, "a", NA), 2015) # NAs
#'
#' @family epi calendar functions
#'

epi_week_date <- function(epi_week, epi_year, offset = 0, system = "who") {
  df_full <- dplyr::data_frame(epi_week, epi_year)
  na_rows_input <- nrow(df_full) - sum(complete.cases(df_full))
  df_distinct <- df_full %>% dplyr::distinct()
  z <- mapply(function(x, y) {
    epi_year_start(y) + ((x -1) * 7)
  }, suppressWarnings(as.numeric(df_distinct$epi_week)),
  suppressWarnings(as.numeric(df_distinct$epi_year)))
  class(z) <- "Date"  # sapply unlists and drops the class, so put it back
  z[epi_week(z)$epi_week != df_distinct$epi_week &
      epi_week(z, system)$epi_year != df_distinct$epi_year] <- NA
  df_distinct <- df_distinct %>% mutate(epi_date = z + offset)
  df_full <- suppressMessages(
    df_full %>% left_join(df_distinct)
  )
  na_rows_output <- nrow(df_full) - sum(complete.cases(df_full))
  nas_created <- na_rows_output - na_rows_input
  if (nas_created) {
    message("Some epi-weeks could not be coerced to dates: ",
            nas_created, " additional NAs returned")
  }
  return(df_full$epi_date)
}
