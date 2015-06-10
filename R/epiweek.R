
epi_week1_day1 <- function(x) {

  # The first epidemiological week of year ends on the first Saturday of
  # January, provided that it falls at least four days into the month.

  require(lubridate)

  # determine the date of the first saturday of the year
  year_day1 <- as.Date(paste(x, "01-01", sep="-"))
  year_week1 <- seq(year_day1, length.out=7, by="day")
  year_saturday1 <- year_week1[wday(year_week1) == 7]

  # determine the first day of the first epi-week of the year
  if(yday(year_saturday1) >= 4) {
    year_saturday1 - 6 # the previous Sunday
  } else {
    year_saturday1 + 1 # the following Sunday
  }

}