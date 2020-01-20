
library(tibble)
library(lubridate)

start_date <- "2020-01-01"
end_date <- "2022-04-01"

reporting_calendar_table <- tibble(
    ID = 1:{seq.Date(as.Date(start_date), as.Date(end_date), by = "month") %>% length()},
    StartDate = seq.Date(as.Date(start_date), as.Date(end_date), by = "month"),
    EndDate = rollback(StartDate + 31)
)
