# FlexFile Submission 1: Contract Award ---------------------------------------

cerberus_submission_1 <- flexfile

cerberus_submission_1$ReportConfiguration <- list(
    DetailedStandardCategory = report_configuration_table %>%
        filter(Field == "Detailed Standard Category") %>%
        pull(Value),
    GA_AsStandardCategory = report_configuration_table %>%
        filter(Field == "GA as Standard Category") %>%
        pull(Value),
    FCCM_AsStandardCategory = report_configuration_table %>%
        filter(Field == "FCCM as Standard Category") %>%
        pull(Value),
    ForecastAtCompletion_ByNonrecurringOrRecurring = report_configuration_table %>%
        filter(Field == "FAC by Nonrecurring/Recurring") %>%
        pull(Value),
    ForecastAtCompletion_ByStandardCategory = report_configuration_table %>%
        filter(Field == "FAC by Standard Category") %>%
        pull(Value)
)

cerberus_submission_1$ReportMetadata <- ReportMetadata

cerberus_submission_1$ReportMetadata$ContractPrice <- NA # Create based on FAC + some %

# PoP start and end for submission 1 are the same since it is as of contract award.
cerberus_submission_1$ReportMetadata$PeriodOfPerformance_StartDate <- order_or_lots_table %>%
    slice(1) %>%
    pull(PeriodOfPerformance_StartDate)

cerberus_submission_1$ReportMetadata$PeriodOfPerformance_EndDate <- order_or_lots_table %>%
    slice(1) %>%
    pull(PeriodOfPerformance_StartDate)

cerberus_submission_1$ReportMetadata$SubmissionEvent_Number <- 1L

cerberus_submission_1$ReportMetadata$ReportCycleID <- events_table %>%
    filter(EventID == cerberus_submission_1$ReportMetadata$SubmissionEvent_Number) %>%
    pull(ReportCycle)

cerberus_submission_1$ReportMetadata$SubmissionEvent_Name <- events_table %>%
    filter(EventID == cerberus_submission_1$ReportMetadata$SubmissionEvent_Number) %>%
    pull(SubmissionEventName)

# ReportAsOf for the first submission is the StartDate (rather than EndDate)
# since it occurs the date of award.
cerberus_submission_1$ReportMetadata$ReportAsOf <- order_or_lots_table %>%
    slice(1) %>%
    pull(PeriodOfPerformance_StartDate)

cerberus_submission_1$OrdersOrLots <-
    cerberus_submission_1$OrdersOrLots %>%
    right_join(order_or_lots_table)

# The date preapred for the initial submission is before its as of date since it
# would have been prepared for the proposal which clearly has to have been
# completed prior to contract award.
cerberus_submission_1$ReportMetadata$DatePrepared <- order_or_lots_table %>%
    slice(1) %>%
    pull(PeriodOfPerformance_StartDate) - 15

# Filters on `StartDate` rather than `EndDate` since the first submission is the
# date.
cerberus_submission_1$ReportMetadata$ReportingPeriodID <- reporting_calendar_table %>%
    filter(StartDate == cerberus_submission_1$ReportMetadata$ReportAsOf) %>%
    pull(ID)

cerberus_submission_1$CLINs <- clin_table %>%
    filter(str_detect(ID, pattern = "^0"))

cerberus_submission_1$EndItems <- end_item_table

cerberus_submission_1$WBS <- wbs_table

cerberus_submission_1$Accounts # Will be empty since $0 have been incurred at contract award

cerberus_submission_1$FunctionalCategories <-
    functional_categories_table %>%
    select(-detailed_standard_category_id)

cerberus_submission_1$FunctionalOverheadCategories <-
    functional_overhead_categories_table

cerberus_submission_1$UnitsOrSublots # Will be empty since $0 have been incurred at contract award

cerberus_submission_1$ReportingCalendar <- reporting_calendar_table

cerberus_submission_1$SummaryCostData

cerberus_submission_1$ActualCostHourData # Will be empty since $0 have been incurred at contract award

cerberus_submission_1$ForecastAtCompletionCostHourData

cerberus_submission_1$AllocationMethods

cerberus_submission_1$AllocationComponents

cerberus_submission_1$SummaryRemarks # Will be empty since $0 have been incurred at contract award

cerberus_submission_1$WBSElementRemarks <-
    wbs_element_remarks %>% filter(OrderOrLotID %in% c("1_BA"))


cerberus_submission_1$WBSDictionaryDefinitions <- wbs_dictionary_definitions_table

cerberus_submission_1$CostHourTagDefinitions # Empty since not using any tags

# FlexFile Submission 3: Delivery of all First Article Test Vehicles ----------

cerberus_submission_3 <- flexfile

cerberus_submission_3$ReportConfiguration <- cerberus_submission_1$ReportConfiguration

cerberus_submission_3$ReportMetadata <- ReportMetadata

cerberus_submission_3$ReportMetadata$ContractPrice <- NA # Create based on FAC + some %

cerberus_submission_3$ReportMetadata$PeriodOfPerformance_StartDate <- order_or_lots_table %>%
    slice(1) %>%
    pull(PeriodOfPerformance_StartDate)

cerberus_submission_3$ReportMetadata$PeriodOfPerformance_EndDate <- events_table %>%
    slice(3) %>%
    pull(AsOfDate)

cerberus_submission_3$ReportMetadata$SubmissionEvent_Number <- 3L

cerberus_submission_3$ReportMetadata$ReportCycleID <- events_table %>%
    filter(EventID == cerberus_submission_3$ReportMetadata$SubmissionEvent_Number) %>%
    pull(ReportCycle)

cerberus_submission_3$ReportMetadata$SubmissionEvent_Name <- events_table %>%
    filter(EventID == cerberus_submission_3$ReportMetadata$SubmissionEvent_Number) %>%
    pull(SubmissionEventName)

cerberus_submission_3$ReportMetadata$ReportAsOf <- order_or_lots_table %>%
    slice(2) %>%
    pull(PeriodOfPerformance_EndDate)

cerberus_submission_3$OrdersOrLots <-
    cerberus_submission_3$OrdersOrLots %>%
    right_join(order_or_lots_table)

cerberus_submission_3$ReportMetadata$DatePrepared <- order_or_lots_table %>%
    slice(2) %>%
    pull(PeriodOfPerformance_EndDate) + 15

cerberus_submission_3$ReportMetadata$ReportingPeriodID <- reporting_calendar_table %>%
    filter(EndDate == cerberus_submission_3$ReportMetadata$ReportAsOf) %>%
    pull(ID)

cerberus_submission_3$OrdersOrLots <-
    cerberus_submission_3$OrdersOrLots %>%
    right_join(order_or_lots_table)

cerberus_submission_3$CLINs <-
    clin_table %>%
    filter(str_detect(ID, pattern = "^0") |
           str_detect(ID, pattern = "^1"))

cerberus_submission_3$EndItems <- end_item_table

cerberus_submission_3$WBS <- wbs_table

cerberus_submission_3$Accounts

cerberus_submission_3$FunctionalCategories <-
    functional_categories_table %>%
    select(-detailed_standard_category_id)

cerberus_submission_3$FunctionalOverheadCategories <-
    functional_overhead_categories_table

cerberus_submission_3$UnitsOrSublots <- units_or_sublots %>%
    filter(OrderOrLotID %in% c("1_BA", "2_L1"))

cerberus_submission_3$ReportingCalendar <- reporting_calendar_table

cerberus_submission_3$SummaryCostData

cerberus_submission_3$ActualCostHourData

cerberus_submission_3$ForecastAtCompletionCostHourData

cerberus_submission_3$AllocationMethods

cerberus_submission_3$AllocationComponents

cerberus_submission_3$SummaryRemarks <- summary_remarks %>%
    filter(OrderOrLotID %in% c("1_BA", "2_L1"))

cerberus_submission_3$WBSElementRemarks <-
    wbs_element_remarks %>% filter(OrderOrLotID %in% c("1_BA", "2_L1"))

cerberus_submission_3$WBSDictionaryDefinitions <- wbs_dictionary_definitions_table

cerberus_submission_3$CostHourTagDefinitions # Empty since not using any tags

# FlexFile Submission 5: Complete LRIP Deliveries -----------------------------

cerberus_submission_5 <- flexfile

cerberus_submission_5$ReportConfiguration <- cerberus_submission_1$ReportConfiguration

cerberus_submission_5$ReportMetadata <- ReportMetadata

cerberus_submission_5$ReportMetadata$ContractPrice <- NA # Create based on FAC + some %

cerberus_submission_5$ReportMetadata$PeriodOfPerformance_StartDate <- order_or_lots_table %>%
    slice(2) %>%
    pull(PeriodOfPerformance_StartDate)

cerberus_submission_5$ReportMetadata$PeriodOfPerformance_EndDate <- events_table %>%
    slice(5) %>%
    pull(AsOfDate)

cerberus_submission_5$ReportMetadata$SubmissionEvent_Number <- 5L

cerberus_submission_5$ReportMetadata$ReportCycleID <- events_table %>%
    filter(EventID == cerberus_submission_5$ReportMetadata$SubmissionEvent_Number) %>%
    pull(ReportCycle)

cerberus_submission_5$ReportMetadata$SubmissionEvent_Name <- events_table %>%
    filter(EventID == cerberus_submission_5$ReportMetadata$SubmissionEvent_Number) %>%
    pull(SubmissionEventName)

cerberus_submission_5$ReportMetadata$ReportAsOf <- order_or_lots_table %>%
    slice(3) %>%
    pull(PeriodOfPerformance_EndDate)

cerberus_submission_5$OrdersOrLots <-
    cerberus_submission_5$OrdersOrLots %>%
    right_join(order_or_lots_table)

cerberus_submission_5$ReportMetadata$DatePrepared <- order_or_lots_table %>%
    slice(3) %>%
    pull(PeriodOfPerformance_EndDate) + 15

cerberus_submission_5$ReportMetadata$ReportingPeriodID <- reporting_calendar_table %>%
    filter(EndDate == cerberus_submission_5$ReportMetadata$ReportAsOf) %>%
    pull(ID)

cerberus_submission_5$OrdersOrLots <-
    cerberus_submission_5$OrdersOrLots %>%
    right_join(order_or_lots_table)

cerberus_submission_5$CLINs <-
    clin_table %>%
    filter(str_detect(ID, pattern = "^0") |
           str_detect(ID, pattern = "^1") |
           str_detect(ID, pattern = "^2"))

cerberus_submission_5$EndItems <- end_item_table

cerberus_submission_5$WBS <- wbs_table

cerberus_submission_5$Accounts

cerberus_submission_5$FunctionalCategories <-
    functional_categories_table %>%
    select(-detailed_standard_category_id)

cerberus_submission_5$FunctionalOverheadCategories <-
    functional_overhead_categories_table

cerberus_submission_5$UnitsOrSublots <- units_or_sublots %>%
    filter(OrderOrLotID %in% c("1_BA", "2_L1", "3_L2"))

cerberus_submission_5$ReportingCalendar <- reporting_calendar_table

cerberus_submission_5$SummaryCostData

cerberus_submission_5$ActualCostHourData

cerberus_submission_5$ForecastAtCompletionCostHourData

cerberus_submission_5$AllocationMethods

cerberus_submission_5$AllocationComponents

cerberus_submission_5$SummaryRemarks <- summary_remarks %>%
    filter(OrderOrLotID %in% c("1_BA", "2_L1", "3_L2"))

cerberus_submission_5$WBSElementRemarks <-
    wbs_element_remarks %>% filter(OrderOrLotID %in% c("1_BA", "2_L1", "3_L2"))

cerberus_submission_5$WBSDictionaryDefinitions <- wbs_dictionary_definitions_table

cerberus_submission_5$CostHourTagDefinitions  # Empty since not using any tags


# FlexFile Submission 7: Deliveries/Contract Complete -------------------------

cerberus_submission_7 <- flexfile

cerberus_submission_7$ReportConfiguration <- cerberus_submission_1$ReportConfiguration

cerberus_submission_7$ReportMetadata <- ReportMetadata

cerberus_submission_7$ReportMetadata$ContractPrice <- NA # Create based on FAC + some %

cerberus_submission_7$ReportMetadata$PeriodOfPerformance_StartDate <- order_or_lots_table %>%
    slice(4) %>%
    pull(PeriodOfPerformance_StartDate)

cerberus_submission_7$ReportMetadata$PeriodOfPerformance_EndDate <- events_table %>%
    slice(7) %>%
    pull(AsOfDate)

cerberus_submission_7$ReportMetadata$SubmissionEvent_Number <- 7L

cerberus_submission_7$ReportMetadata$ReportCycleID <- events_table %>%
    filter(EventID == cerberus_submission_7$ReportMetadata$SubmissionEvent_Number) %>%
    pull(ReportCycle)

cerberus_submission_7$ReportMetadata$SubmissionEvent_Name <- events_table %>%
    filter(EventID == cerberus_submission_7$ReportMetadata$SubmissionEvent_Number) %>%
    pull(SubmissionEventName)

cerberus_submission_7$ReportMetadata$ReportAsOf <- order_or_lots_table %>%
    slice(5) %>%
    pull(PeriodOfPerformance_EndDate)

cerberus_submission_7$OrdersOrLots <-
    cerberus_submission_7$OrdersOrLots %>%
    right_join(order_or_lots_table)

cerberus_submission_7$ReportMetadata$DatePrepared <- order_or_lots_table %>%
    slice(5) %>%
    pull(PeriodOfPerformance_EndDate) + 15

cerberus_submission_7$ReportMetadata$ReportingPeriodID <- reporting_calendar_table %>%
    filter(EndDate == cerberus_submission_7$ReportMetadata$ReportAsOf) %>%
    pull(ID)

cerberus_submission_7$OrdersOrLots <-
    cerberus_submission_7$OrdersOrLots %>%
    right_join(order_or_lots_table)

cerberus_submission_7$CLINs <-
    clin_table %>%
    filter(str_detect(ID, pattern = "^0") |
           str_detect(ID, pattern = "^1") |
           str_detect(ID, pattern = "^2") |
           str_detect(ID, pattern = "^3") |
           str_detect(ID, pattern = "^4")
           )

cerberus_submission_7$EndItems <- end_item_table

cerberus_submission_7$WBS <- wbs_table

cerberus_submission_7$Accounts

cerberus_submission_7$FunctionalCategories <-
    functional_categories_table %>%
    select(-detailed_standard_category_id)

cerberus_submission_7$FunctionalOverheadCategories <-
    functional_overhead_categories_table

cerberus_submission_7$UnitsOrSublots <- units_or_sublots

cerberus_submission_7$ReportingCalendar <- reporting_calendar_table

cerberus_submission_7$SummaryCostData

cerberus_submission_7$ActualCostHourData

cerberus_submission_7$ForecastAtCompletionCostHourData

cerberus_submission_7$AllocationMethods

cerberus_submission_7$AllocationComponents

cerberus_submission_7$SummaryRemarks <- summary_remarks

cerberus_submission_7$WBSElementRemarks <- wbs_element_remarks

cerberus_submission_7$WBSDictionaryDefinitions <- wbs_dictionary_definitions_table

cerberus_submission_7$CostHourTagDefinitions # Empty since not using any tags

