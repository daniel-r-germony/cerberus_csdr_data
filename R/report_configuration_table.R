
library(magrittr)
library(tibble)
library(dplyr)

report_configuration_table <- tribble(
    ~Field,                          ~Value,
    "Detailed Standard Category",    FALSE,
    "GA as Standard Category",       FALSE,
    "FCCM as Standard Category",     FALSE,
    "FAC by Nonrecurring/Recurring", TRUE,
    "FAC by Standard Category",      TRUE
)

# # The code below will take the above table (which is what the worksheet looks
# # like in the Excel three part template) and converts it into a list object
# # which is a more accurate assessment of how the data model treats the data.
# #
# # This is currently commented out b/c this actual table to list converstion is
# # being completed in the "create_submissions.R" file but it is included here for
# # reference.
# ReportConfiguration <- list(
#     DetailedStandardCategory = report_configuration_table %>%
#         filter(Field == "Detailed Standard Category") %>%
#         select(Value) %>%
#         pull(-1),
#     GA_AsStandardCategory = report_configuration_table %>%
#         filter(Field == "GA as Standard Category") %>%
#         select(Value) %>%
#         pull(-1),
#     FCCM_AsStandardCategory = report_configuration_table %>%
#         filter(Field == "FCCM as Standard Category") %>%
#         select(Value) %>%
#         pull(-1),
#     ForecastAtCompletion_ByNonrecurringOrRecurring = report_configuration_table %>%
#         filter(Field == "FAC by Nonrecurring/Recurring") %>%
#         select(Value) %>%
#         pull(-1),
#     ForecastAtCompletion_ByStandardCategory = report_configuration_table %>%
#         filter(Field == "FAC by Standard Category") %>%
#         select(Value) %>%
#         pull(-1)
# )
