
library(magrittr)
library(dplyr)

# # Can stick this at the end of any of the below assignments to get a quick
# # view of what cost looks like by month.
# %>%  ggplot(aes(StartDate, Value_Hours)) + geom_col()

# RECURRING -------------------------------------------------------------------
# 11-1000 Top Executives -----

hours_executive_rec <- "11-1000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 2020 / 12 * 0.75,
        max = 2020 / 12 * 1.25
    ) %>%
    mutate(
        NonrecurringOrRecurringID = "RECURRING"
    )

# 11-2000 Advertising, Marketing, Promotions, Public Relations, and Sales Managers -----

hours_sales_rec <- "11-2000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 2020 / 12 * 0.35,
        max = 2020 / 12 * 0.85
    ) %>%
    mutate(
        NonrecurringOrRecurringID = "RECURRING"
    )

# 11-3000 Operations Specialties Managers -----

hours_ops_mgmr_rec <- "11-2000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 2020 / 12 * 0.35,
        max = 2020 / 12 * 0.85
    ) %>%
    mutate(
        NonrecurringOrRecurringID = "RECURRING"
    )

# 11-9000 Other Management Occupations -----

hours_other_mgmt_rec <- "11-9000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 1.5,
        shape2 = 3
    ) %>%
    mutate(
        Value_Hours = Value_Hours * 100 *
            runif(n = length(Value_Hours),
                  min = 0.90,
                  max =  1.10),
        NonrecurringOrRecurringID = "RECURRING"
    )

# 13-1000 Business Operations Specialists -----

hours_biz_ops_rec <- "13-1000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 2020 / 12 * 0.75,
        max = 2020 / 12 * 1.25
    ) %>%
    mutate(
        NonrecurringOrRecurringID = "RECURRING"
    )

# 13-2000 Financial Specialists -----

hours_finance_rec <- "13-2000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 2020 / 12 * 0.75,
        max = 2020 / 12 * 1.25
    ) %>%
    mutate(
        NonrecurringOrRecurringID = "RECURRING"
    )

# 15-1100 Computer Occupations -----

hours_eng_cs_rec <- "15-1100" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 1,
        shape2 = 1.25
    ) %>% mutate(
        Value_Hours = Value_Hours * 5 *
            runif(n = length(Value_Hours),
                  min = 0.85,
                  max =  1.15),
        NonrecurringOrRecurringID = "RECURRING"
    )

# 15-2000 Mathematical Science Occupations -----

hours_eng_math_rec <- "15-2000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 1.15,
        shape2 = 1.35
    ) %>% mutate(
        Value_Hours = Value_Hours * 3 *
            runif(n = length(Value_Hours),
                  min = 0.85,
                  max =  1.15),
        NonrecurringOrRecurringID = "RECURRING"
    )

# 17-2000 Engineers -----

hours_eng_rec <- "17-2000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 1.0,
        shape2 = 1.35
    ) %>% mutate(
        Value_Hours = Value_Hours * 3 *
            runif(n = length(Value_Hours),
                  min = 0.85,
                  max =  1.15),
        NonrecurringOrRecurringID = "RECURRING"
    )

# 17-3000 Drafters, Engineering Technicians, and Mapping Technicians -----

hours_eng_tech_rec <- "17-3000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 0.7,
        shape2 = 1.35
    ) %>% mutate(
        Value_Hours = Value_Hours * 3 *
            runif(n = length(Value_Hours),
                  min = 0.85,
                  max =  1.15),
        NonrecurringOrRecurringID = "RECURRING"
    )

# 27-1000 Art and Design Workers -----

hours_design_rec <- "27-1000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 1,
        max = 1
    ) %>% mutate(
        Value_Hours = Value_Hours * 0,
        NonrecurringOrRecurringID = "RECURRING")

# 27-3000 Media and Communication Workers -----

hours_comm_rec <- "27-3000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 30,
        max = 80
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 27-4000 Media and Communication Equipment Workers -----

hours_comm_equ_rec <- "27-4000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 60,
        max = 80
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 31-2000 Occupational Therapy and Physical Therapist Assistants and Aides -----

hours_ot_rec <- "31-2000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 32/2,
        max = 40/2
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 31-9000 Other Healthcare Support Occupations -----

hours_other_heath_rec <- "31-9000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 32/4,
        max = 40/4
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 41-1000 Supervisors of Sales Workers -----

hours_sales_super_rec <- "41-1000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 32,
        max = 40
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 41-3000 Sales Representatives, Services -----

hours_sales_field_rec <- "41-3000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 32 * 2,
        max = 40 * 2
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 41-4000 Sales Representatives, Wholesale and Manufacturing -----

hours_sales_ws_rec <- "41-4000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 32 * 2,
        max = 40 * 2
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 41-9000 Other Sales and Related Workers -----

hours_sales_other_rec <- "41-9000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 32 * 1.5,
        max = 40 * 1.5
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 43-1000 Supervisors of Office and Administrative Support Workers -----

hours_office_supers_rec <- "43-1000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 32 * 1,
        max = 40 * 1
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 43-3000 Financial Clerks -----

hours_finance_clerks_rec <- "43-3000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 32 * 2.5,
        max = 40 * 2.5
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 43-4000 Information and Record Clerks -----

hours_other_clerks_rec <- "43-4000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 32 * 0.5,
        max = 40 * 0.5
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 43-5000 Material Recording, Scheduling, Dispatching, and Distributing Workers -----

hours_records_rec <- "43-5000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 32 * 0.5,
        max = 40 * 0.5
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 43-6000 Secretaries and Administrative Assistants -----

hours_admin_assistant_rec <- "43-6000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 32 * 2.5,
        max = 40 * 3.5
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 43-9000 Other Office and Administrative Support Workers -----

hours_other_office_rec <- "43-9000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 32 * 0.5,
        max = 40 * 0.5
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 47-1000 Supervisors of Construction and Extraction Workers -----

hours_const_supers_rec <- "47-1000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 47-2000 Construction Trades Workers -----

hours_const_trade_rec <- "47-2000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 47-3000 Helpers, Construction Trades -----

hours_const_support_rec <- "47-3000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 47-4000 Other Construction and Related Workers -----

hours_const_other_rec <- "47-4000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 49-1000 Supervisors of Installation, Maintenance, and Repair Workers -----

hours_mx_supers_rec <- "49-1000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 32 * 1.5,
        max = 40 * 1.5
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 49-2000 Electrical and Electronic Equipment Mechanics, Installers, and Repairers -----

hours_mx_elec_rec <- "49-2000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 32 * 2,
        max = 40 * 2
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 49-3000 Vehicle and Mobile Equipment Mechanics, Installers, and Repairers -----

hours_mx_vehicle_rec <- "49-3000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 32 * 10,
        max = 40 * 10
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 49-9000 Other Installation, Maintenance, and Repair Occupations -----

hours_mx_other_rec <- "49-9000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 32 * 2.5,
        max = 40 * 2.5
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 51-1000 Supervisors of Production Workers -----

hours_prod_supers_rec <- "51-1000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 32 * 3,
        max = 40 * 3
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 51-2000 Assemblers and Fabricators -----

hours_prod_assembly_rec <- "51-2000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 1.5,
        shape2 = 1.3
    ) %>%
    mutate(
        Value_Hours = Value_Hours * 300 *
            runif(n = length(Value_Hours),
                  min = 0.90,
                  max =  1.10),
        NonrecurringOrRecurringID = "RECURRING"
    )

# 51-4000 Metal Workers and Plastic Workers -----

hours_prod_welding_rec <- "51-4000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 1.2,
        shape2 = 1.3
    ) %>%
    mutate(
        Value_Hours = Value_Hours * 50 *
            runif(n = length(Value_Hours),
                  min = 0.90,
                  max =  1.10),
        NonrecurringOrRecurringID = "RECURRING"
    )

# 51-5100 Printing Workers -----

hours_prod_paint_rec <- "51-5100" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 1.6,
        shape2 = 1.3
    ) %>%
    mutate(
        Value_Hours = Value_Hours * 30 *
            runif(n = length(Value_Hours),
                  min = 0.90,
                  max =  1.10),
        NonrecurringOrRecurringID = "RECURRING"
    )
# 51-8000 Plant and System Operators -----

hours_prod_ops_rec <- "51-8000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 32 * 2,
        max = 40 * 2
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 51-9000 Other Production Occupations -----

hours_prod_other <- "51-9000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 1.5,
        shape2 = 1.3
    ) %>%
    mutate(
        Value_Hours = Value_Hours * 50 *
            runif(n = length(Value_Hours),
                  min = 0.90,
                  max =  1.10),
        NonrecurringOrRecurringID = "RECURRING"
    )

# 53-1000 Supervisors of Transportation and Material Moving Workers -----

hours_mat_supers_rec <- "53-1000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 32 * 2,
        max = 40 * 2
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# 53-3000 Motor Vehicle Operators -----

hours_mx_operators_rec <- "53-3000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 7,
        shape2 = 2
    ) %>%
    mutate(
        Value_Hours = Value_Hours * 50 *
            runif(n = length(Value_Hours),
                  min = 0.90,
                  max =  1.10),
        NonrecurringOrRecurringID = "RECURRING"
    )

# 53-6000 Other Transportation Workers -----

hours_mat_other_rec <- "53-6000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 7,
        shape2 = 2
    ) %>%
    mutate(
        Value_Hours = Value_Hours * 3 *
            runif(n = length(Value_Hours),
                  min = 0.90,
                  max =  1.10),
        NonrecurringOrRecurringID = "RECURRING"
    )

# 53-7000 Material Moving Workers -----

hours_mat_movers_rec <- "53-7000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 32 * 4,
        max = 40 * 4
    ) %>% mutate(
        NonrecurringOrRecurringID = "RECURRING")

# Bind RECURRING hours into one tibble and remove 0 values -----

recurring_hours <- bind_rows(
    hours_executive_rec,
    hours_sales_rec,
    hours_ops_mgmr_rec,
    hours_other_mgmt_rec,
    hours_biz_ops_rec,
    hours_finance_rec,
    hours_eng_cs_rec,
    hours_eng_math_rec,
    hours_eng_rec,
    hours_eng_tech_rec,
    hours_design_rec,
    hours_comm_rec,
    hours_comm_equ_rec,
    hours_ot_rec,
    hours_other_heath_rec,
    hours_sales_super_rec,
    hours_sales_field_rec,
    hours_sales_ws_rec,
    hours_sales_other_rec,
    hours_office_supers_rec,
    hours_finance_clerks_rec,
    hours_other_clerks_rec,
    hours_records_rec,
    hours_admin_assistant_rec,
    hours_other_office_rec,
    hours_const_supers_rec,
    hours_const_trade_rec,
    hours_const_support_rec,
    hours_const_other_rec,
    hours_mx_supers_rec,
    hours_mx_elec_rec,
    hours_mx_vehicle_rec,
    hours_mx_other_rec,
    hours_prod_supers_rec,
    hours_prod_assembly_rec,
    hours_prod_welding_rec,
    hours_prod_paint_rec,
    hours_prod_ops_rec,
    hours_prod_other,
    hours_mat_supers_rec,
    hours_mx_operators_rec,
    hours_mat_other_rec,
    hours_mat_movers_rec
) %>% filter(Value_Hours > 0)
