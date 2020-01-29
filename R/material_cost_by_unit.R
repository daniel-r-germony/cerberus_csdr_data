
library(magrittr)
library(dplyr)
library(tidyr)

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
    pivot_longer(cols = 7:11, names_to = "detailed_standard_category_id",values_to = "VALUE_DOLLARS") %>%
    select(-percent_complete) %>% ungroup() %>% filter(!is.na(VALUE_DOLLARS)) %>%
    mutate(end_item_qty = LastUnitNumber - FirstUnitNumber + 1,
           unit_cost = VALUE_DOLLARS / end_item_qty,
           fac_VALUE_DOLLARS = VALUE_DOLLARS * runif(length(VALUE_DOLLARS), 0.95, 1.30))

# # Creates a table to show the cost by month and detailed_standard_category_id
# material_cost %>%
#     ggplot(aes(FirstUnitNumber, VALUE_DOLLARS)) +
#     geom_col(aes(fill = detailed_standard_category_id)) +
#     facet_wrap(detailed_standard_category_id ~ EndItemID)

# # No idea why I wrote this or what I was looking for.
# units_or_sublots %>%
#     group_by(EndItemID, OrderOrLotID) %>%
#     mutate(percent_complete = FirstUnitNumber / max(LastUnitNumber)) %>%
#     mutate(test_col = case_when(
#                ID == "5" ~ 850 * length(percent_complete),
#                TRUE ~ NA_real_)) %>% filter(!is.na(test_col)) %>% print(n = Inf)
