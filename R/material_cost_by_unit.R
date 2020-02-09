
library(magrittr)
library(dplyr)
library(tidyr)

#     $ OrderOrLotID                 <chr>
#     $ CLIN_ID                      <fct>
#     $ EndItemID                    <chr> Done
#     $ WBSElementID                 <chr>
#     $ AccountID                    <chr>
#     $ NonrecurringOrRecurringID    <fct> Done
#     $ FunctionalCategoryID         <chr>
#     $ FunctionalOverheadCategoryID <chr>
#     $ StandardCategoryID           <fct> N/A
#     $ DetailedStandardCategoryID   <fct> Done
#     $ UnitOrSublotID               <chr>
#     $ AllocationMethodID           <chr>
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
    pivot_longer(cols = 7:11, names_to = "DetailedStandardCategoryID",values_to = "VALUE_DOLLARS") %>%
    select(-percent_complete) %>% ungroup() %>% filter(!is.na(VALUE_DOLLARS)) %>%
    mutate(end_item_qty = LastUnitNumber - FirstUnitNumber + 1,
           unit_cost = VALUE_DOLLARS / end_item_qty,
           fac_VALUE_DOLLARS = VALUE_DOLLARS * runif(length(VALUE_DOLLARS), 0.95, 1.30))

# # Creates a table to show the cost by month and DetailedStandardCategoryID
# material_cost %>%
#     ggplot(aes(FirstUnitNumber, VALUE_DOLLARS)) +
#     geom_col(aes(fill = DetailedStandardCategoryID)) +
#     facet_wrap(DetailedStandardCategoryID ~ EndItemID)

# # No idea why I wrote this or what I was looking for.
# units_or_sublots %>%
#     group_by(EndItemID, OrderOrLotID) %>%
#     mutate(percent_complete = FirstUnitNumber / max(LastUnitNumber)) %>%
#     mutate(test_col = case_when(
#                ID == "5" ~ 850 * length(percent_complete),
#                TRUE ~ NA_real_)) %>% filter(!is.na(test_col)) %>% print(n = Inf)

gp_wbs_distro <- tibble::tribble(
                                 ~ID,                                           ~Name, ~PURCHASED_PARTS, ~PURCHASED_EQUIPMENT, ~RAW_MATERIALS, ~INTERCOMPANY_WORK_ORDERS, ~DIRECT_REPORTING_SUBCONTRACTOR,
                           "1.1.1.1", "CAV Integration, Assembly, Test, and Checkout",                0,                    0,            0.1,                         0,                               0,
                           "1.1.1.2",                           "Hull/Frame/Body/Cab",              0.1,                    0,            0.4,                         0,                               0,
                           "1.1.1.3",                          "System Survivability",              0.1,                 0.15,            0.1,                         0,                               0,
                           "1.1.1.4",                               "Turret Assembly",                0,                    0,              0,                         0,                               0,
                           "1.1.1.5",                           "Suspension/Steering",             0.25,                  0.2,           0.15,                         0,                               0,
                           "1.1.1.6",                           "Vehicle Electronics",             0.15,                  0.1,            0.1,                         0,                               0,
                     "1.1.1.7.1.1.1",                                  "Engine Block",              0.2,                  0.1,           0.05,                         0,                               0,
                     "1.1.1.7.1.1.2",                                  "Turbocharger",             0.05,                    0,              0,                         0,                               0,
                     "1.1.1.7.1.1.3",                          "Other Dressed Engine",                0,                    0,              0,                         0,                               0,
                       "1.1.1.7.1.2",                                  "Transmission",                0,                 0.15,           0.05,                         0,                               0,
                         "1.1.1.7.2",                                   "Drive Train",                0,                 0.05,              0,                         0,                               0,
                           "1.1.1.8",                          "Auxiliary Automotive",                0,                    0,           0.05,                         0,                               0,
                           "1.1.1.9",                                  "Fire Control",                0,                    0,              0,                         0,                               0,
                          "1.1.1.10",                                      "Armament",                0,                    0,              0,                         0,                               0,
                          "1.1.1.11",                 "Automatic Ammunition Handling",                0,                    0,              0,                         0,                               0,
                          "1.1.1.12",        "Navigation and Remote Piloting Systems",             0.15,                  0.2,              0,                         0,                               0,
                          "1.1.1.13",                             "Special Equipment",                0,                    0,              0,                         0,                               0,
                          "1.1.1.14",                                "Communications",                0,                 0.05,              0,                         0,                               0,
                          "1.1.1.15",                          "CAV Software Release",                0,                    0,              0,                         0,                               0,
                          "1.1.1.16",                           "Other CAV Subsystem",                0,                    0,              0,                         0,                               0
                     ) %>%
    pivot_longer(cols = -c(1:2),
                 names_to = "DetailedStandardCategoryID",
                 values_to = "PercentValue") %>%
    filter(PercentValue > 0)

c2_wbs_distro <- tibble::tribble(
                                 ~ID,                                           ~Name, ~PURCHASED_PARTS, ~PURCHASED_EQUIPMENT, ~RAW_MATERIALS, ~INTERCOMPANY_WORK_ORDERS, ~DIRECT_REPORTING_SUBCONTRACTOR,
                           "1.1.1.1", "CAV Integration, Assembly, Test, and Checkout",                0,                    0,            0.1,                         0,                               0,
                           "1.1.1.2",                           "Hull/Frame/Body/Cab",              0.1,                    0,            0.4,                         0,                               0,
                           "1.1.1.3",                          "System Survivability",              0.1,                 0.15,            0.1,                         0,                               0,
                           "1.1.1.4",                               "Turret Assembly",                0,                    0,              0,                         0,                               0,
                           "1.1.1.5",                           "Suspension/Steering",             0.25,                  0.2,           0.15,                         0,                               0,
                           "1.1.1.6",                           "Vehicle Electronics",             0.15,                  0.1,            0.1,                         0,                               0,
                     "1.1.1.7.1.1.1",                                  "Engine Block",              0.2,                  0.1,           0.05,                         0,                               0,
                     "1.1.1.7.1.1.2",                                  "Turbocharger",             0.05,                    0,              0,                         0,                               0,
                     "1.1.1.7.1.1.3",                          "Other Dressed Engine",                0,                    0,              0,                         0,                               0,
                       "1.1.1.7.1.2",                                  "Transmission",                0,                 0.15,           0.05,                         0,                               0,
                         "1.1.1.7.2",                                   "Drive Train",                0,                 0.05,              0,                         0,                               0,
                           "1.1.1.8",                          "Auxiliary Automotive",                0,                    0,           0.05,                       0.2,                               0,
                           "1.1.1.9",                                  "Fire Control",                0,                    0,              0,                         0,                               0,
                          "1.1.1.10",                                      "Armament",                0,                    0,              0,                         0,                               0,
                          "1.1.1.11",                 "Automatic Ammunition Handling",                0,                    0,              0,                         0,                               0,
                          "1.1.1.12",        "Navigation and Remote Piloting Systems",             0.15,                  0.2,              0,                         0,                               0,
                          "1.1.1.13",                             "Special Equipment",                0,                    0,              0,                       0.3,                               0,
                          "1.1.1.14",                                "Communications",                0,                 0.05,              0,                       0.5,                               0,
                          "1.1.1.15",                          "CAV Software Release",                0,                    0,              0,                         0,                               0,
                          "1.1.1.16",                           "Other CAV Subsystem",                0,                    0,              0,                         0,                               0
                     ) %>%
    pivot_longer(cols = -c(1:2),
                 names_to = "DetailedStandardCategoryID",
                 values_to = "PercentValue") %>%
    filter(PercentValue > 0)

hw_wbs_distro <- tibble::tribble(
                                 ~ID,                                           ~Name, ~PURCHASED_PARTS, ~PURCHASED_EQUIPMENT, ~RAW_MATERIALS, ~INTERCOMPANY_WORK_ORDERS, ~DIRECT_REPORTING_SUBCONTRACTOR,
                           "1.1.1.1", "CAV Integration, Assembly, Test, and Checkout",                0,                    0,            0.1,                         0,                               0,
                           "1.1.1.2",                           "Hull/Frame/Body/Cab",              0.1,                    0,            0.4,                         0,                               0,
                           "1.1.1.3",                          "System Survivability",              0.1,                 0.15,            0.1,                         0,                               0,
                           "1.1.1.4",                               "Turret Assembly",                0,                    0,              0,                         0,                               0,
                           "1.1.1.5",                           "Suspension/Steering",             0.25,                  0.2,           0.15,                         0,                               0,
                           "1.1.1.6",                           "Vehicle Electronics",             0.15,                  0.1,            0.1,                         0,                               0,
                     "1.1.1.7.1.1.1",                                  "Engine Block",              0.2,                  0.1,           0.05,                         0,                               0,
                     "1.1.1.7.1.1.2",                                  "Turbocharger",             0.05,                    0,              0,                         0,                               0,
                     "1.1.1.7.1.1.3",                          "Other Dressed Engine",                0,                    0,              0,                         0,                               0,
                       "1.1.1.7.1.2",                                  "Transmission",                0,                 0.15,           0.05,                         0,                               0,
                         "1.1.1.7.2",                                   "Drive Train",                0,                 0.05,              0,                         0,                               0,
                           "1.1.1.8",                          "Auxiliary Automotive",                0,                    0,           0.05,                         0,                               0,
                           "1.1.1.9",                                  "Fire Control",                0,                    0,              0,                       0.4,                               0,
                          "1.1.1.10",                                      "Armament",                0,                    0,              0,                      0.15,                               1,
                          "1.1.1.11",                 "Automatic Ammunition Handling",                0,                    0,              0,                      0.35,                               0,
                          "1.1.1.12",        "Navigation and Remote Piloting Systems",             0.15,                  0.2,              0,                         0,                               0,
                          "1.1.1.13",                             "Special Equipment",                0,                    0,              0,                       0.1,                               0,
                          "1.1.1.14",                                "Communications",                0,                 0.05,              0,                         0,                               0,
                          "1.1.1.15",                          "CAV Software Release",                0,                    0,              0,                         0,                               0,
                          "1.1.1.16",                           "Other CAV Subsystem",                0,                    0,              0,                         0,                               0
                     ) %>%
    pivot_longer(cols = -c(1:2),
                 names_to = "DetailedStandardCategoryID",
                 values_to = "PercentValue") %>%
    filter(PercentValue > 0)

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
    select(-.CLIN_ID) %>%
    print(n = Inf)

