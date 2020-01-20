
units_or_sublots %>%
    group_by(OrderOrLotID, ID) %>%
    summarise(Qty = max(LastUnitNumber) - min(FirstUnitNumber) + 1) %>%
    pivot_wider(names_from = OrderOrLotID, values_from = Qty) %>%
    print(n = Inf)

units_or_sublots %>% group_by(EndItemID) %>%
    mutate(pct_complete = FirstUnitNumber / max(LastUnitNumber)) %>%
    mutate(DIRECT_REPORTING_SUBCONTRACTOR =
               case_when(str_detect(ID, pattern = "/*_") ~
                             stats::dbeta(x = pct_complete,
                                          shape1 = 1,
                                          shape2 = 2),
                         TRUE ~ NA_real_),
           INTERCOMPANY_WORK_ORDERS =
               case_when(
                   ID == "3_CTV_C2" ~ 10000 + 5000 *
                       stats::dbeta(x = pct_complete,
                                    shape1 = 1,
                                    shape2 = 3) *
                       runif(length(pct_complete), 0.75, 1.35),
                   ID == "4_CTV_HW" ~ 10000 + 7500 *
                       stats::dbeta(x = pct_complete,
                                    shape1 = 1,
                                    shape2 = 2) *
                       runif(length(pct_complete), 0.75, 1.35),
                   TRUE ~ NA_real_),
            PURCHASED_PARTS =
               case_when(
                   ID == "2_CTV_GP" ~ 112500 + 10000 *
                       stats::dbeta(x = pct_complete,
                                    shape1 = 1,
                                    shape2 = 2) *
                       runif(length(pct_complete), 0.75, 1.35),
                   ID == "3_CTV_C2" ~ 125000 + 20000 *
                       stats::dbeta(x = pct_complete,
                                    shape1 = 1,
                                    shape2 = 2) *
                       runif(length(pct_complete), 0.75, 1.35),
                   ID == "4_CTV_HW" ~ 135000 + 25000 *
                       stats::dbeta(x = pct_complete,
                                    shape1 = 1,
                                    shape2 = 2) *
                       runif(length(pct_complete), 0.75, 1.35),
                   ID == "5" ~ 2500,
                   ID == "6" ~ 3500,
                   ID == "7" ~ 4000,
                   TRUE ~ NA_real_),
           PURCHASED_EQUIPMENT =
               case_when(
                   ID == "2_CTV_GP" ~ 10000 + 5000 *
                       stats::dbeta(x = pct_complete,
                                    shape1 = 1,
                                    shape2 = 2) *
                       runif(length(pct_complete), 0.75, 1.35),
                   ID == "3_CTV_C2" ~ 12000 + 7000 *
                       stats::dbeta(x = pct_complete,
                                    shape1 = 1,
                                    shape2 = 2) *
                       runif(length(pct_complete), 0.75, 1.35),
                   ID == "4_CTV_HW" ~ 15000 + 9000 *
                       stats::dbeta(x = pct_complete,
                                    shape1 = 1,
                                    shape2 = 2) *
                       runif(length(pct_complete), 0.75, 1.35),
                   ID == "5" ~ 2500,
                   ID == "6" ~ 3500,
                   ID == "7" ~ 4000,
                   TRUE ~ NA_real_),
           RAW_MATERIALS =
               case_when(
                   ID == "2_CTV_GP" ~ 10000 *
                       stats::dbeta(x = pct_complete,
                                    shape1 = 1,
                                    shape2 = 2) *
                       runif(length(pct_complete), 0.75, 1.35),
                   ID == "3_CTV_C2" ~ 10000 *
                       stats::dbeta(x = pct_complete,
                                    shape1 = 1,
                                    shape2 = 2) *
                       runif(length(pct_complete), 0.75, 1.35),
                   ID == "4_CTV_HW" ~ 10000 *
                       stats::dbeta(x = pct_complete,
                                    shape1 = 1,
                                    shape2 = 2) *
                       runif(length(pct_complete), 0.75, 1.35),
                   TRUE ~ NA_real_)) %>%
    ggplot(aes(FirstUnitNumber, PURCHASED_PARTS)) +
    geom_col() +
    facet_wrap(~ EndItemID)

* runif(n = tally(ID == "3_CTV_C2") %>% as.integer(), min = 0.9, max = 1.15)
