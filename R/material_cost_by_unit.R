
library(magrittr)
library(dplyr)
library(tidyr)

#     $ OrderOrLotID                 <chr> Done
#     $ CLIN_ID                      <fct> Done
#     $ EndItemID                    <chr> Done
#     $ WBSElementID                 <chr> Done
#     $ AccountID                    <chr>
#     $ NonrecurringOrRecurringID    <fct> Done
#     $ FunctionalCategoryID         <chr> Done
#     $ FunctionalOverheadCategoryID <chr> Done
#     $ StandardCategoryID           <fct> N/A
#     $ DetailedStandardCategoryID   <fct> Done
#     $ UnitOrSublotID               <chr> N/A?
#     $ AllocationMethodID           <chr> N/A
#     $ ReportingPeriodID            <int>
#     $ Tag1                         <chr> N/A
#     $ ...                                N/A
#     $ Tag25                        <chr> N/A
#     $ Value_Dollars                <dbl> Done
#     $ Value_Hours                  <dbl>

material_cost <- units_or_sublots %>%
    group_by(EndItemID, OrderOrLotID) %>%
    mutate(percent_complete = FirstUnitNumber / max(LastUnitNumber)) %>%
    mutate(DIRECT_REPORTING_SUBCONTRACTOR =
               case_when(
                   ID == "4_CTV_HW" ~ 10000, # fixed unit cost for DRS parts
                   TRUE ~ NA_real_),
           INTERCOMPANY_WORK_ORDERS =
               case_when(
                   ID == "3_CTV_C2" ~ 10000 + 2000 *
                       dbeta(x = percent_complete, shape1 = 1, shape2 = 3) *
                       runif(length(percent_complete), 0.75, 1.35),
                   ID == "4_CTV_HW" ~ 10000 + 3500 *
                       dbeta(x = percent_complete, shape1 = 1, shape2 = 2) *
                       runif(length(percent_complete), 0.75, 1.35),
                   TRUE ~ NA_real_),
            PURCHASED_PARTS =
               case_when(
                   ID == "2_CTV_GP" ~ 11250 + 1000 *
                       dbeta(x = percent_complete, shape1 = 1, shape2 = 2) *
                       runif(length(percent_complete), 0.75, 1.35),
                   ID == "3_CTV_C2" ~ 12500 + 1000 *
                       dbeta(x = percent_complete, shape1 = 1, shape2 = 2) *
                       runif(length(percent_complete), 0.75, 1.35),
                   ID == "4_CTV_HW" ~ 13500 + 1000 *
                       dbeta(x = percent_complete, shape1 = 1, shape2 = 2) *
                       runif(length(percent_complete), 0.75, 1.35),
                   ID == "5" ~ 850 * runif(length(percent_complete), 0.95, 1.15) * length(percent_complete),
                   ID == "6" ~ 550 * runif(length(percent_complete), 0.95, 1.15) * length(percent_complete),
                   ID == "7" ~ 900 * runif(length(percent_complete), 0.95, 1.15) * length(percent_complete),
                   TRUE ~ NA_real_),
           PURCHASED_EQUIPMENT =
               case_when(
                   ID == "2_CTV_GP" ~ 10000 + 1000 *
                       dbeta(x = percent_complete, shape1 = 1, shape2 = 2) *
                       runif(length(percent_complete), 0.95, 1.05),
                   ID == "3_CTV_C2" ~ 12000 + 1100 *
                       dbeta(x = percent_complete, shape1 = 1, shape2 = 2) *
                       runif(length(percent_complete), 0.95, 1.05),
                   ID == "4_CTV_HW" ~ 15000 + 1200 *
                       dbeta(x = percent_complete, shape1 = 1, shape2 = 2) *
                       runif(length(percent_complete), 0.95, 1.05),
                   ID == "5" ~ 1500 * runif(length(percent_complete), 0.95, 1.15) * length(percent_complete),
                   ID == "6" ~ 2000 * runif(length(percent_complete), 0.95, 1.15) * length(percent_complete),
                   ID == "7" ~ 4000 * runif(length(percent_complete), 0.95, 1.15) * length(percent_complete),
                   TRUE ~ NA_real_),
           RAW_MATERIALS =
               case_when(
                   ID == "2_CTV_GP" ~ 13000 + 2000 *
                       dbeta(x = percent_complete, shape1 = 1, shape2 = 2) *
                       runif(length(percent_complete), 0.85, 1.45),
                   ID == "3_CTV_C2" ~ 15000 + 2100 *
                       dbeta(x = percent_complete, shape1 = 1, shape2 = 2) *
                       runif(length(percent_complete), 0.85, 1.45),
                   ID == "4_CTV_HW" ~ 10000 + 2350 *
                       dbeta(x = percent_complete, shape1 = 1, shape2 = 2) *
                       runif(length(percent_complete), 0.85, 1.45),
                   ID == "5" ~ 3000 * runif(length(percent_complete), 0.75, 1.35) * length(percent_complete),
                   ID == "6" ~ 2500 * runif(length(percent_complete), 0.75, 1.35) * length(percent_complete),
                   ID == "7" ~ 1000 * runif(length(percent_complete), 0.75, 1.35) * length(percent_complete),
                   TRUE ~ NA_real_)) %>%
    pivot_longer(cols = 7:11, names_to = "DetailedStandardCategoryID",values_to = "Value_Dollars") %>%
    select(-percent_complete) %>% ungroup() %>% filter(!is.na(Value_Dollars)) %>%
    mutate(end_item_qty = LastUnitNumber - FirstUnitNumber + 1,
           unit_cost = round(Value_Dollars / end_item_qty, 2),
           fac_Value_Dollars = round(Value_Dollars * runif(length(Value_Dollars), 0.95, 1.30), 2),
           Value_Dollars = round(Value_Dollars, 2))

# # Creates a table to show the cost by month and DetailedStandardCategoryID
# material_cost %>%
#     ggplot(aes(FirstUnitNumber, Value_Dollars)) +
#     geom_col(aes(fill = DetailedStandardCategoryID)) +
#     facet_wrap(DetailedStandardCategoryID ~ EndItemID)

# # No idea why I wrote this or what I was looking for.
# units_or_sublots %>%
#     group_by(EndItemID, OrderOrLotID) %>%
#     mutate(percent_complete = FirstUnitNumber / max(LastUnitNumber)) %>%
#     mutate(test_col = case_when(
#                ID == "5" ~ 850 * length(percent_complete),
#                TRUE ~ NA_real_)) %>% filter(!is.na(test_col)) %>% print(n = Inf)

material_cost <- material_cost %>% mutate(NonrecurringOrRecurringID = "RECURRING")

material_cost <- material_cost %>%
    left_join(clin_table_plus %>% select(CLIN_ID = ID, EndItemID, OrderOrLotID),
              by = c("OrderOrLotID" = "OrderOrLotID",
                     "EndItemID" = "EndItemID"))

# This case_when fills in the CLIN_ID for the kits. It has to create a new
# .CLIN_ID column which is later dropped b/c you can't case_when into a <fct>
# column. Instead, a <chr> version is created as .CLIN_ID which then replaces
# the CLIN_ID column but inherits its factor levels.
material_cost <- material_cost %>% mutate(.CLIN_ID = case_when(
    EndItemID %in% c("Combat Bumper Kit", "Fording Kit", "Winch Kit") &
        str_starts(OrderOrLotID, "1") ~
        "0004",
    EndItemID %in% c("Combat Bumper Kit", "Fording Kit", "Winch Kit") &
    str_starts(OrderOrLotID, "2") ~
        "1004",
    EndItemID %in% c("Combat Bumper Kit", "Fording Kit", "Winch Kit") &
    str_starts(OrderOrLotID, "3") ~
        "2004",
    EndItemID %in% c("Combat Bumper Kit", "Fording Kit", "Winch Kit") &
    str_starts(OrderOrLotID, "4") ~
        "3004",
    EndItemID %in% c("Combat Bumper Kit", "Fording Kit", "Winch Kit") &
    str_starts(OrderOrLotID, "5") ~
        "4004",
    TRUE ~ as.character(CLIN_ID)
)) %>% mutate(CLIN_ID = .CLIN_ID %>% factor(levels = levels(material_cost$CLIN_ID))) %>%
    select(-.CLIN_ID)

material_cost <- material_cost %>% mutate(FunctionalCategoryID = case_when(
    DetailedStandardCategoryID == "PURCHASED_PARTS" ~ "99-0002",
    DetailedStandardCategoryID == "PURCHASED_EQUIPMENT" ~ "99-0003",
    DetailedStandardCategoryID == "RAW_MATERIALS" ~ "99-0001",
    DetailedStandardCategoryID == "INTERCOMPANY_WORK_ORDERS" ~ "99-0000",
    DetailedStandardCategoryID == "DIRECT_REPORTING_SUBCONTRACTOR" ~ "99-0002",
    TRUE ~ NA_character_
)) %>% mutate(FunctionalOverheadCategoryID = "99")

# Create fake DetailedStandardCategoryID to WBSElementID distributions that will
# be multiplied against our faked material unit costs to give us a material cost
# by WBS and DetailedStandardCategory. Might need to come back and fine tune
# these to ensure WBSElements/DetailedStandardCategory that are common between
# variants have approximately the same unit cost.

gp_wbs_distro <- tibble::tribble(
    ~WBSElementID,                                   ~WBSElementName, ~PURCHASED_PARTS, ~PURCHASED_EQUIPMENT, ~RAW_MATERIALS, ~INTERCOMPANY_WORK_ORDERS, ~DIRECT_REPORTING_SUBCONTRACTOR,
    "1.1.1.1",       "CAV Integration, Assembly, Test, and Checkout",                0,                    0,           0.09,                         0,                               0,
    "1.1.1.2",                                 "Hull/Frame/Body/Cab",             0.12,                    0,           0.40,                         0,                               0,
    "1.1.1.3",                                "System Survivability",             0.08,                 0.15,           0.11,                         0,                               0,
    "1.1.1.4",                                     "Turret Assembly",                0,                    0,              0,                         0,                               0,
    "1.1.1.5",                                 "Suspension/Steering",             0.25,                 0.18,           0.15,                         0,                               0,
    "1.1.1.6",                                 "Vehicle Electronics",             0.15,                 0.11,           0.10,                         0,                               0,
    "1.1.1.7.1.1.1",                                  "Engine Block",             0.20,                 0.09,           0.04,                         0,                               0,
    "1.1.1.7.1.1.2",                                  "Turbocharger",             0.05,                    0,              0,                         0,                               0,
    "1.1.1.7.1.1.3",                          "Other Dressed Engine",                0,                    0,              0,                         0,                               0,
    "1.1.1.7.1.2",                                    "Transmission",                0,                 0.15,           0.06,                         0,                               0,
    "1.1.1.7.2",                                       "Drive Train",                0,                 0.05,              0,                         0,                               0,
    "1.1.1.8",                                "Auxiliary Automotive",                0,                    0,           0.05,                         0,                               0,
    "1.1.1.9",                                        "Fire Control",                0,                    0,              0,                         0,                               0,
    "1.1.1.10",                                           "Armament",                0,                    0,              0,                         0,                               0,
    "1.1.1.11",                      "Automatic Ammunition Handling",                0,                    0,              0,                         0,                               0,
    "1.1.1.12",             "Navigation and Remote Piloting Systems",             0.15,                 0.22,              0,                         0,                               0,
    "1.1.1.13",                                  "Special Equipment",                0,                    0,              0,                         0,                               0,
    "1.1.1.14",                                     "Communications",                0,                 0.05,              0,                         0,                               0,
    "1.1.1.15",                               "CAV Software Release",                0,                    0,              0,                         0,                               0,
    "1.1.1.16",                                "Other CAV Subsystem",                0,                    0,              0,                         0,                               0
) %>%
    pivot_longer(cols = -c(1:2),
                 names_to = "DetailedStandardCategoryID",
                 values_to = "PercentValue") %>%
    filter(PercentValue > 0) %>%
    mutate(EndItemID = "2_CTV_GP")

c2_wbs_distro <- tibble::tribble(
    ~WBSElementID,                             ~WBSElementName, ~PURCHASED_PARTS, ~PURCHASED_EQUIPMENT, ~RAW_MATERIALS, ~INTERCOMPANY_WORK_ORDERS, ~DIRECT_REPORTING_SUBCONTRACTOR,
    "1.1.1.1", "CAV Integration, Assembly, Test, and Checkout",                0,                    0,           0.09,                         0,                               0,
    "1.1.1.2",                           "Hull/Frame/Body/Cab",             0.09,                    0,           0.40,                         0,                               0,
    "1.1.1.3",                          "System Survivability",             0.11,                 0.14,           0.11,                         0,                               0,
    "1.1.1.4",                               "Turret Assembly",                0,                    0,              0,                         0,                               0,
    "1.1.1.5",                           "Suspension/Steering",             0.25,                 0.21,           0.15,                         0,                               0,
    "1.1.1.6",                           "Vehicle Electronics",             0.15,                 0.09,           0.10,                         0,                               0,
    "1.1.1.7.1.1.1",                            "Engine Block",             0.20,                 0.11,           0.05,                         0,                               0,
    "1.1.1.7.1.1.2",                            "Turbocharger",             0.05,                    0,              0,                         0,                               0,
    "1.1.1.7.1.1.3",                    "Other Dressed Engine",                0,                    0,              0,                         0,                               0,
    "1.1.1.7.1.2",                              "Transmission",                0,                 0.16,           0.03,                         0,                               0,
    "1.1.1.7.2",                                 "Drive Train",                0,                 0.04,              0,                         0,                               0,
    "1.1.1.8",                          "Auxiliary Automotive",                0,                    0,           0.07,                      0.20,                               0,
    "1.1.1.9",                                  "Fire Control",                0,                    0,              0,                         0,                               0,
    "1.1.1.10",                                     "Armament",                0,                    0,              0,                         0,                               0,
    "1.1.1.11",                "Automatic Ammunition Handling",                0,                    0,              0,                         0,                               0,
    "1.1.1.12",       "Navigation and Remote Piloting Systems",             0.15,                 0.19,              0,                         0,                               0,
    "1.1.1.13",                            "Special Equipment",                0,                    0,              0,                      0.30,                               0,
    "1.1.1.14",                               "Communications",                0,                 0.06,              0,                      0.50,                               0,
    "1.1.1.15",                         "CAV Software Release",                0,                    0,              0,                         0,                               0,
    "1.1.1.16",                          "Other CAV Subsystem",                0,                    0,              0,                         0,                               0
) %>%
    pivot_longer(cols = -c(1:2),
                 names_to = "DetailedStandardCategoryID",
                 values_to = "PercentValue") %>%
    filter(PercentValue > 0) %>%
    mutate(EndItemID = "3_CTV_C2")

hw_wbs_distro <- tibble::tribble(
    ~WBSElementID,                           ~WBSElementName,   ~PURCHASED_PARTS, ~PURCHASED_EQUIPMENT, ~RAW_MATERIALS, ~INTERCOMPANY_WORK_ORDERS, ~DIRECT_REPORTING_SUBCONTRACTOR,
    "1.1.1.1", "CAV Integration, Assembly, Test, and Checkout",                0,                    0,           0.08,                         0,                               0,
    "1.1.1.2",                           "Hull/Frame/Body/Cab",             0.09,                    0,           0.40,                         0,                               0,
    "1.1.1.3",                          "System Survivability",             0.11,                 0.15,           0.12,                         0,                               0,
    "1.1.1.4",                               "Turret Assembly",                0,                    0,              0,                         0,                               0,
    "1.1.1.5",                           "Suspension/Steering",             0.25,                 0.19,           0.15,                         0,                               0,
    "1.1.1.6",                           "Vehicle Electronics",             0.15,                 0.11,           0.10,                         0,                               0,
    "1.1.1.7.1.1.1",                            "Engine Block",             0.20,                 0.09,           0.04,                         0,                               0,
    "1.1.1.7.1.1.2",                            "Turbocharger",             0.05,                    0,              0,                         0,                               0,
    "1.1.1.7.1.1.3",                    "Other Dressed Engine",                0,                    0,              0,                         0,                               0,
    "1.1.1.7.1.2",                              "Transmission",                0,                 0.16,           0.05,                         0,                               0,
    "1.1.1.7.2",                                 "Drive Train",                0,                 0.06,              0,                         0,                               0,
    "1.1.1.8",                          "Auxiliary Automotive",                0,                    0,           0.06,                         0,                               0,
    "1.1.1.9",                                  "Fire Control",                0,                    0,              0,                      0.40,                               0,
    "1.1.1.10",                                     "Armament",                0,                    0,              0,                      0.15,                            1.00,
    "1.1.1.11",                "Automatic Ammunition Handling",                0,                    0,              0,                      0.35,                               0,
    "1.1.1.12",       "Navigation and Remote Piloting Systems",             0.15,                 0.20,              0,                         0,                               0,
    "1.1.1.13",                            "Special Equipment",                0,                    0,              0,                      0.10,                               0,
    "1.1.1.14",                               "Communications",                0,                 0.04,              0,                         0,                               0,
    "1.1.1.15",                         "CAV Software Release",                0,                    0,              0,                         0,                               0,
    "1.1.1.16",                          "Other CAV Subsystem",                0,                    0,              0,                         0,                               0
) %>%
    pivot_longer(cols = -c(1:2),
                 names_to = "DetailedStandardCategoryID",
                 values_to = "PercentValue") %>%
    filter(PercentValue > 0) %>%
    mutate(EndItemID = "4_CTV_HW")

wbs_distro_table <- bind_rows(gp_wbs_distro, c2_wbs_distro)
wbs_distro_table <- bind_rows(wbs_distro_table, hw_wbs_distro)

# Left join the above created distro tables into the overall material_cost
# table. Since distro tables only exist for the prime vehicles,
# mutate/case_whens are used to fill in the left_joined columns for kits. Then
# multiply the PercentValue column by the unit_cost and fac_Value_Dollars
# columns to finish the disto of cost by WBSElement.

material_cost <- left_join(material_cost,
                           wbs_distro_table,
                           by = c("ID" = "EndItemID",
                                  "DetailedStandardCategoryID" = "DetailedStandardCategoryID")
                  ) %>%
    # TODO: Seems like there should be a better way to lookup the equipment kit WBS.
    mutate(WBSElementID = case_when(
        EndItemID %>% str_ends(" Kit") ~ "1.1.3",
        TRUE ~ WBSElementID
    )) %>%
    mutate(PercentValue = case_when(
        EndItemID %>% str_ends(" Kit") ~ 1.0,
        TRUE ~ PercentValue
    )) %>%
    # TODO: For sure there is a better way to lookup the equipment kit WBS name vs. a hardcoded value (left_join?).
    mutate(WBSElementName = case_when(
        WBSElementID == "1.1.3" ~ "Equipment Kits",
        TRUE ~ WBSElementName
    )) %>%
    mutate(Value_Dollars = Value_Dollars * PercentValue,
           fac_Value_Dollars = fac_Value_Dollars * PercentValue)


    # Drop columns we no longer need and sort into ActualCostHourData order.
material_cost_for_part2 <- material_cost %>%
    select(
           OrderOrLotID,
           CLIN_ID,
           EndItemID = ID,
           WBSElementID,
           # AccountID, # Will generate these with the labor data to prevent unintended duplicates
           NonrecurringOrRecurringID,
           FunctionalCategoryID,
           FunctionalOverheadCategoryID,
           # StandardCategoryID, # Will be blank since we are using DetailedStandardCategoryID
           DetailedStandardCategoryID,
           # UnitOrSublotID, # Not sure what this is...
           # AllocationMethodID, # Unused
           # ReportingPeriodID, # is month incurred... need to figure this out
           Value_Dollars,
           # Value_Hours, # Will get from the labor table
           -WBSElementName,
           -PercentValue,
           -end_item_qty,
           -unit_cost,
           -EndItemID,
           -FirstUnitNumber,
           -LastUnitNumber,
           -fac_Value_Dollars # Will need this for part 3
    )

material_cost_for_part3 <- material_cost %>%
    select(
        OrderOrLotID,
        -CLIN_ID,
        -ID,
        WBSElementID,
        # AccountID, # Will generate these with the labor data to prevent unintended duplicates
        NonrecurringOrRecurringID,
        -FunctionalCategoryID,
        -FunctionalOverheadCategoryID,
        # StandardCategoryID, # Will be blank since we are using DetailedStandardCategoryID
        DetailedStandardCategoryID,
        # UnitOrSublotID, # Not sure what this is...
        # AllocationMethodID, # Unused
        # ReportingPeriodID, # is month incurred... need to figure this out
        -Value_Dollars,
        # Value_Hours, # Will get from the labor table
        -WBSElementName,
        -PercentValue,
        -end_item_qty,
        -unit_cost,
        -EndItemID,
        -FirstUnitNumber,
        -LastUnitNumber,
        Value_Dollars = fac_Value_Dollars # Will need this for part 3
    ) %>%
    group_by(
        OrderOrLotID,
        WBSElementID,
        NonrecurringOrRecurringID,
        DetailedStandardCategoryID
    ) %>%
    summarise(Value_Dollars = sum(Value_Dollars))
