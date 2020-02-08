
library(charlatan)
library(dplyr)
library(magrittr)

#     $ OrderOrLotID                 <chr> DONE
#     $ CLIN_ID                      <fct>
#     $ EndItemID                    <chr>
#     $ WBSElementID                 <chr>
#     $ AccountID                    <chr> DONE
#     $ NonrecurringOrRecurringID    <fct> DONE
#     $ FunctionalCategoryID         <chr> DONE
#     $ FunctionalOverheadCategoryID <chr> DONE
#     $ StandardCategoryID           <fct> N/A
#     $ DetailedStandardCategoryID   <fct> DONE
#     $ UnitOrSublotID               <chr>
#     $ AllocationMethodID           <chr>
#     $ ReportingPeriodID            <int> DONE
#     $ Tag1                         <chr> N/A
#     $ ...                                N/A
#     $ Tag25                        <chr> N/A
#     $ Value_Dollars                <dbl> DONE
#     $ Value_Hours                  <dbl> DONE

combined_hours <- bind_rows(nonrecurring_hours, recurring_hours)

# Add AccountID with random credit card numbers (credit card numbers look like
# they could be something called "account" to me)
combined_hours <- combined_hours %>%
    mutate(AccountID = ch_credit_card_number(nrow(combined_hours)))

# Mutate an OrderOrLotID column into the reporting calendar (will be used in a
# min to map OrderOrLotID into the combined hours table))
# TODO: This needs to be refactored with a map function so it will work with an
# unlimited number of order lots
reporting_calendar_table_plus <- reporting_calendar_table %>%
    mutate(OrderOrLotID = case_when(
        StartDate >= order_or_lots_table$PeriodOfPerformance_StartDate[1] &
        EndDate   <= order_or_lots_table$PeriodOfPerformance_EndDate[1] ~
            order_or_lots_table$ID[1],
        StartDate >= order_or_lots_table$PeriodOfPerformance_StartDate[2] &
        EndDate   <= order_or_lots_table$PeriodOfPerformance_EndDate[2] ~
            order_or_lots_table$ID[2],
        StartDate >= order_or_lots_table$PeriodOfPerformance_StartDate[3] &
        EndDate   <= order_or_lots_table$PeriodOfPerformance_EndDate[3] ~
            order_or_lots_table$ID[3],
        StartDate >= order_or_lots_table$PeriodOfPerformance_StartDate[4] &
        EndDate   <= order_or_lots_table$PeriodOfPerformance_EndDate[4] ~
            order_or_lots_table$ID[4],
        StartDate >= order_or_lots_table$PeriodOfPerformance_StartDate[5] &
        EndDate   <= order_or_lots_table$PeriodOfPerformance_EndDate[5] ~
            order_or_lots_table$ID[5],
        TRUE ~ NA_character_
    ))

# Mutate OrderOrLotID into the combined_hours table.
combined_hours <- reporting_calendar_table_plus %>%
    select(ID, OrderOrLotID) %>%
    left_join(combined_hours, ., by = c("ReportingPeriodID" = "ID"))

# Mutate FunctionalOverheadCategoryID into the combined_hours table.
combined_hours <- combined_hours %>%
    mutate(FunctionalOverheadCategoryID = ID) %>%
    separate(
        data = .,
        col = FunctionalOverheadCategoryID,
        into = c("FunctionalOverheadCategoryID", ".drop"),
        sep = "-") %>% select(-.drop)

# Mutate CLIN_ID with SEPM and Test CLINs crosswalked (vehicle and kits will need an allocation)
combined_hours <- combined_hours %>%
    left_join(functional_categories_with_clin_table %>% select(-Name, -detailed_standard_category_id),
              by = c("ID" = "ID")) %>%
    left_join(clin_table_plus %>% select(-ContractTypeID, CLIN_ID = ID, Name),
              by = c("OrderOrLotID" = "OrderOrLotID", "CLIN_Name" = "Name")) %>%
    select(-CLIN_Name)
