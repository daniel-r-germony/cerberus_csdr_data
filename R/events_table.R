
# MESSAGE: The below tribble is very wide (177 characters).

# TODO: This table can/should be imported right from a CSDR Plan.
# TODO: The DataReport column should likly be turned into a nested list of factors (only a given set of report names can be used and an unknown number of reports can be listed in each column so having a nested list seems reaonable).

# THOUGHT: It is odd that this table from the CSDR Plan does not get ported over in some way to the three part template.

events_table <- tibble::tribble(
    ~EventID, ~DataReport,                                       ~SubmissionEventName,                          ~ReportCycle, ~AsOfDate,                ~DueDate,
    1L,	"Cost and Hour Report (FlexFile), Quantity Data Report", "Contract Award",	                            "Initial",    lubridate::ymd(20200101), lubridate::ymd(20200302),
    2L,	"Technical Data Report",	                             "Contract Award",                              "Initial",    lubridate::ymd(20200101), lubridate::ymd(20200302),
    3L,	"Cost and Hour Report (FlexFile), Quantity Data Report", "Delivery of all First Article Test Vehicles",	"Interim",    lubridate::ymd(20200831), lubridate::ymd(20201031),
    4L,	"Technical Data Report",	                             "Delivery of all First Article Test Vehicles",	"Interim",    lubridate::ymd(20200831), lubridate::ymd(20201031),
    5L,	"Cost and Hour Report (FlexFile), Quantity Data Report", "Complete LRIP Deliveries",	                "Interim",    lubridate::ymd(20210228), lubridate::ymd(20210430),
    6L,	"Technical Data Report",	                             "Complete LRIP Deliveries",	                "Interim",    lubridate::ymd(20210228), lubridate::ymd(20210430),
    7L,	"Cost and Hour Report (FlexFile), Quantity Data Report", "Deliveries/Contract Complete",	            "Final",      lubridate::ymd(20220430), lubridate::ymd(20220630),
    8L,	"Technical Data Report",	                             "Deliveries/Contract Complete",	            "Final",      lubridate::ymd(20220430), lubridate::ymd(20220630)
)

# The data model's ReportCycleEnum requires the ReportCycle value to be
# uppercase.  The below code converts the cPet produced lower case ReportCycle
# to upper and converts them to a factor since that's how the FlexFile S3 class
# expects to see them.

events_table <- events_table %>%
    mutate(ReportCycle = case_when(
        ReportCycle == "Initial" | ReportCycle == "Initial" ~ "INITIAL",
        ReportCycle == "Interim" | ReportCycle == "INTERIM" ~ "INTERIM",
        ReportCycle == "Final"   | ReportCycle == "FINAL"   ~ "FINAL",
        TRUE ~ NA_character_
        )) %>%
    mutate(ReportCycle = factor(ReportCycle,
                                levels = c("INITIAL",
                                           "INTERIM",
                                           "FINAL"),
                                ordered = TRUE)
           )
