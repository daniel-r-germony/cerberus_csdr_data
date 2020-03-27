
library(charlatan)
library(dplyr)
library(magrittr)

#     $ OrderOrLotID                 <chr> DONE
#     $ CLIN_ID                      <fct> started
#     $ EndItemID                    <chr> started
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

combined_hours <- combined_hours %>%
    mutate(WBSElementID = "1.1.1.1",
           AccountID = NA_character_,
           StandardCategoryID = NA_character_,
           UnitOrSublotID = NA_character_,
           AllocationMethodID = NA_character_,
           ReportingPeriodID = NA_character_,
           Tag1 = NA_character_,
           Tag2 = NA_character_,
           Tag3 = NA_character_,
           Tag4 = NA_character_,
           Tag5 = NA_character_,
           Tag6 = NA_character_,
           Tag7 = NA_character_,
           Tag8 = NA_character_,
           Tag9 = NA_character_,
           Tag10 = NA_character_,
           Tag11 = NA_character_,
           Tag12 = NA_character_,
           Tag13 = NA_character_,
           Tag14 = NA_character_,
           Tag15 = NA_character_,
           Tag16 = NA_character_,
           Tag17 = NA_character_,
           Tag18 = NA_character_,
           Tag19 = NA_character_,
           Tag20 = NA_character_,
           Tag21 = NA_character_,
           Tag22 = NA_character_,
           Tag23 = NA_character_,
           Tag24 = NA_character_,
           Tag25 = NA_character_
           ) %>%
    select(OrderOrLotID,
           CLIN_ID,
           EndItemID,
           WBSElementID,
           AccountID,
           NonrecurringOrRecurringID,
           FunctionalCategoryID = Name,
           FunctionalOverheadCategoryID,
           StandardCategoryID,
           DetailedStandardCategoryID = detailed_standard_category_id,
           UnitOrSublotID,
           AllocationMethodID,
           ReportingPeriodID,
           Tag1,
           Tag2,
           Tag3,
           Tag4,
           Tag5,
           Tag6,
           Tag7,
           Tag8,
           Tag9,
           Tag10,
           Tag11,
           Tag12,
           Tag13,
           Tag14,
           Tag15,
           Tag16,
           Tag17,
           Tag18,
           Tag19,
           Tag20,
           Tag21,
           Tag22,
           Tag23,
           Tag24,
           Tag25,
           Value_Dollars,
           Value_Hours)

actual_cost_data_for_part_2 <-
    bind_rows(combined_hours, material_cost_for_part2) %>%
    # Add AccountID with random credit card numbers (credit card numbers look like
    # they could be something called "account" to me)
    mutate(AccountID = ch_credit_card_number(nrow(.))) %>%
    arrange(OrderOrLotID, CLIN_ID, WBSElementID)

combined_hours_for_part_3 <- combined_hours %>%
    select(
        OrderOrLotID,
        WBSElementID,
        # AccountID, # Will generate these with the labor data to prevent unintended duplicates
        NonrecurringOrRecurringID,
        StandardCategoryID, # Will be blank since we are using DetailedStandardCategoryID
        DetailedStandardCategoryID,
        # UnitOrSublotID, # Not sure what this is...
        # AllocationMethodID, # Unused
        # ReportingPeriodID, # is month incurred... need to figure this out
        Value_Hours, # Will get from the labor table
        Value_Dollars
    ) %>%
    group_by(
        OrderOrLotID,
        WBSElementID,
        NonrecurringOrRecurringID,
        StandardCategoryID,
        DetailedStandardCategoryID
    ) %>%
    summarise(Value_Dollars = sum(Value_Dollars),
              Value_Hours = sum(Value_Hours))

actual_cost_data_for_part_3 <-
    bind_rows(combined_hours_for_part_3, material_cost_for_part3) %>%
    arrange(OrderOrLotID, WBSElementID)

actual_cost_data_for_part_2 %>% filter(OrderOrLotID != "5_F2") %>%
    select(
        OrderOrLotID,
        WBSElementID,
        NonrecurringOrRecurringID,
        StandardCategoryID, # Will be blank since we are using DetailedStandardCategoryID
        DetailedStandardCategoryID,
        Value_Hours,
        Value_Dollars
    ) %>%
    group_by(
        OrderOrLotID,
        WBSElementID,
        NonrecurringOrRecurringID,
        StandardCategoryID,
        DetailedStandardCategoryID
    ) %>%
    summarise(Value_Dollars = sum(Value_Dollars),
              Value_Hours = sum(Value_Hours))
