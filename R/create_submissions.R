# FlexFile Submission 1: Contract Award ---------------------------------------

cerberus_submission_1 <- flexfile

cerberus_submission_1$ReportConfiguration <- list(
    DetailedStandardCategory = report_configuration_table %>%
        filter(Field == "Detailed Standard Category") %>%
        select(Value) %>%
        pull(-1),
    GA_AsStandardCategory = report_configuration_table %>%
        filter(Field == "GA as Standard Category") %>%
        select(Value) %>%
        pull(-1),
    FCCM_AsStandardCategory = report_configuration_table %>%
        filter(Field == "FCCM as Standard Category") %>%
        select(Value) %>%
        pull(-1),
    ForecastAtCompletion_ByNonrecurringOrRecurring = report_configuration_table %>%
        filter(Field == "FAC by Nonrecurring/Recurring") %>%
        select(Value) %>%
        pull(-1),
    ForecastAtCompletion_ByStandardCategory = report_configuration_table %>%
        filter(Field == "FAC by Standard Category") %>%
        select(Value) %>%
        pull(-1)
)

cerberus_submission_1$ReportMetadata

cerberus_submission_1$OrdersOrLots <-
    cerberus_submission_1$OrdersOrLots %>%
    right_join(order_or_lots_table)

cerberus_submission_1$CLINs <-
    clin_table %>%
    filter(str_detect(ID, pattern = "^0"))

cerberus_submission_1$EndItems <- end_item_table

cerberus_submission_1$WBS <- wbs_table

cerberus_submission_1$Accounts

cerberus_submission_1$FunctionalCategories <-
    functional_categories_table %>%
    select(-detailed_standard_category_id)

cerberus_submission_1$FunctionalOverheadCategories

cerberus_submission_1$UnitsOrSublots

cerberus_submission_1$ReportingCalendar

cerberus_submission_1$SummaryCostData

cerberus_submission_1$ActualCostHourData

cerberus_submission_1$ForecastAtCompletionCostHourData

cerberus_submission_1$AllocationMethods

cerberus_submission_1$AllocationComponents

cerberus_submission_1$SummaryRemarks

cerberus_submission_1$WBSElementRemarks

cerberus_submission_1$WBSDictionaryDefinitions

cerberus_submission_1$CostHourTagDefinitions

# FlexFile Submission 3: Delivery of all First Article Test Vehicles ----------

cerberus_submission_3 <- flexfile

cerberus_submission_3$ReportConfiguration <- cerberus_submission_1$ReportConfiguration

cerberus_submission_3$ReportMetadata

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

cerberus_submission_3$FunctionalOverheadCategories

cerberus_submission_3$UnitsOrSublots

cerberus_submission_3$ReportingCalendar

cerberus_submission_3$SummaryCostData

cerberus_submission_3$ActualCostHourData

cerberus_submission_3$ForecastAtCompletionCostHourData

cerberus_submission_3$AllocationMethods

cerberus_submission_3$AllocationComponents

cerberus_submission_3$SummaryRemarks

cerberus_submission_3$WBSElementRemarks

cerberus_submission_3$WBSDictionaryDefinitions

cerberus_submission_3$CostHourTagDefinitions

# FlexFile Submission 5: Complete LRIP Deliveries -----------------------------

cerberus_submission_5 <- flexfile

cerberus_submission_5$ReportConfiguration <- cerberus_submission_1$ReportConfiguration

cerberus_submission_5$ReportMetadata

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

cerberus_submission_5$FunctionalOverheadCategories

cerberus_submission_5$UnitsOrSublots

cerberus_submission_5$ReportingCalendar

cerberus_submission_5$SummaryCostData

cerberus_submission_5$ActualCostHourData

cerberus_submission_5$ForecastAtCompletionCostHourData

cerberus_submission_5$AllocationMethods

cerberus_submission_5$AllocationComponents

cerberus_submission_5$SummaryRemarks

cerberus_submission_5$WBSElementRemarks

cerberus_submission_5$WBSDictionaryDefinitions

cerberus_submission_5$CostHourTagDefinitions


# FlexFile Submission 7: Deliveries/Contract Complete -------------------------

cerberus_submission_7 <- flexfile

cerberus_submission_7$ReportConfiguration <- cerberus_submission_1$ReportConfiguration

cerberus_submission_7$ReportMetadata

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

cerberus_submission_7$FunctionalOverheadCategories

cerberus_submission_7$UnitsOrSublots

cerberus_submission_7$ReportingCalendar

cerberus_submission_7$SummaryCostData

cerberus_submission_7$ActualCostHourData

cerberus_submission_7$ForecastAtCompletionCostHourData

cerberus_submission_7$AllocationMethods

cerberus_submission_7$AllocationComponents

cerberus_submission_7$SummaryRemarks

cerberus_submission_7$WBSElementRemarks

cerberus_submission_7$WBSDictionaryDefinitions

cerberus_submission_7$CostHourTagDefinitions
