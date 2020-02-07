
library(dplyr)
library(lubridate)

inflation_table <- reporting_calendar_table %>%
    mutate(year = year(StartDate),
           month = month(StartDate),
           inflation_rate = (year - min(year) + 1) ^ 0.02)
