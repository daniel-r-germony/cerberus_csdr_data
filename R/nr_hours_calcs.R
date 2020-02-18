
library(magrittr)
library(dplyr)

# # Can stick this at the end of any of the below assignments to get a quick
# # view of what cost looks like by month.
# %>%  ggplot(aes(StartDate, Value_Hours)) + geom_col()

# NONRECURRING -------------------------------------------------------------------
# 11-1000 Top Executives -----

hours_executive_nr <- "11-1000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 11-2000 Advertising, Marketing, Promotions, Public Relations, and Sales Managers -----

hours_sales_nr <- "11-2000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 11-3000 Operations Specialties Managers -----

hours_ops_mgmr_nr <- "11-2000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 11-9000 Other Management Occupations -----

hours_other_mgmt_nr <- "11-9000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 13-1000 Business Operations Specialists -----

hours_biz_ops_nr <- "13-1000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 13-2000 Financial Specialists -----

hours_finance_nr <- "13-2000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 15-1100 Computer Occupations -----

hours_eng_cs_nr <- "15-1100" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 2,
        shape2 = 5
    ) %>% mutate(
        Value_Hours = Value_Hours * 15 *
            runif(n = length(Value_Hours),
                  min = 0.85,
                  max =  1.15)) %>%
            mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
            mutate(Value_Hours = round(Value_Hours, 1))

# 15-2000 Mathematical Science Occupations -----

hours_eng_math_nr <- "15-2000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 1,
        shape2 = 5
    ) %>% mutate(
        Value_Hours = Value_Hours * 6 *
            runif(n = length(Value_Hours),
                  min = 0.85,
                  max =  1.15)) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 17-2000 Engineers -----

hours_eng_nr <- "17-2000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 1.5,
        shape2 = 6.5
    ) %>% mutate(
        Value_Hours = Value_Hours * 50 *
            runif(n = length(Value_Hours),
                  min = 0.85,
                  max =  1.15)) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 17-3000 Drafters, Engineering Technicians, and Mapping Technicians -----

hours_eng_tech_nr <- "17-3000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 2.5,
        shape2 = 5.5
    ) %>% mutate(
        Value_Hours = Value_Hours * 10 *
            runif(n = length(Value_Hours),
                  min = 0.85,
                  max =  1.15)) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 27-1000 Art and Design Workers -----

hours_design_nr <- "27-1000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 3,
        shape2 = 5
    ) %>% mutate(
        Value_Hours = Value_Hours * 5 *
            runif(n = length(Value_Hours),
                  min = 0.85,
                  max =  1.15)) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 27-3000 Media and Communication Workers -----

hours_comm_nr <- "27-3000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 27-4000 Media and Communication Equipment Workers -----

hours_comm_equ_nr <- "27-4000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0) %>%
        mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
        mutate(Value_Hours = round(Value_Hours, 1))

# 31-2000 Occupational Therapy and Physical Therapist Assistants and Aides -----

hours_ot_nr <- "31-2000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 31-9000 Other Healthcare Support Occupations -----

hours_other_heath_nr <- "31-9000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 41-1000 Supervisors of Sales Workers -----

hours_sales_super_nr <- "41-1000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 41-3000 Sales Representatives, Services -----

hours_sales_field_nr <- "41-3000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 41-4000 Sales Representatives, Wholesale and Manufacturing -----

hours_sales_ws_nr <- "41-4000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 41-9000 Other Sales and Related Workers -----

hours_sales_other_nr <- "41-9000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 43-1000 Supervisors of Office and Administrative Support Workers -----

hours_office_supers_nr <- "43-1000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 43-3000 Financial Clerks -----

hours_finance_clerks_nr <- "43-3000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 43-4000 Information and Record Clerks -----

hours_other_clerks_nr <- "43-4000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 43-5000 Material Recording, Scheduling, Dispatching, and Distributing Workers -----

hours_record_nr <- "43-5000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 43-6000 Secretaries and Administrative Assistants -----

hours_admin_assistant_nr <- "43-6000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 43-9000 Other Office and Administrative Support Workers -----

hours_other_office_nr <- "43-9000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 47-1000 Supervisors of Construction and Extraction Workers -----

hours_const_supers_nr <- "47-1000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 1,
        shape2 = 100
    ) %>% mutate(
        Value_Hours = Value_Hours * 10 *
            runif(n = length(Value_Hours),
                  min = 1,
                  max =  1)) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 47-2000 Construction Trades Workers -----

hours_const_trade_nr <- "47-2000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 2,
        shape2 = 20
    ) %>% mutate(
        Value_Hours = Value_Hours * 10 *
            runif(n = length(Value_Hours),
                  min = 1,
                  max =  1)) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 47-3000 Helpers, Construction Trades -----

hours_const_support_nr <- "47-3000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 2,
        shape2 = 20
    ) %>% mutate(
        Value_Hours = Value_Hours * 5 *
            runif(n = length(Value_Hours),
                  min = 1,
                  max =  1)) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 47-4000 Other Construction and Related Workers -----

hours_const_other_nr <- "47-4000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 2,
        shape2 = 10
    ) %>% mutate(
        Value_Hours = Value_Hours * 5 *
            runif(n = length(Value_Hours),
                  min = 1,
                  max =  1)) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 49-1000 Supervisors of Installation, Maintenance, and Repair Workers -----

hours_mx_supers_nr <- "49-1000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>%
        mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
        mutate(Value_Hours = round(Value_Hours, 1))

# 49-2000 Electrical and Electronic Equipment Mechanics, Installers, and Repairers -----

hours_mx_elec_nr <- "49-2000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 49-3000 Vehicle and Mobile Equipment Mechanics, Installers, and Repairers -----

hours_mx_vehicle_nr <- "49-3000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 50,
        shape2 = 250
    ) %>% mutate(
        Value_Hours = Value_Hours * 2 *
            runif(n = length(Value_Hours),
                  min = 1,
                  max =  1)) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 49-9000 Other Installation, Maintenance, and Repair Occupations -----

hours_mx_other_nr <- "49-9000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 51-1000 Supervisors of Production Workers -----

hours_prod_supers_nr <- "51-1000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 51-2000 Assemblers and Fabricators -----

hours_prod_assembly_nr <- "51-2000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 2,
        shape2 = 50
    ) %>%
    mutate(
        Value_Hours = Value_Hours * 7 *
            runif(n = length(Value_Hours),
                  min = 0.90,
                  max =  1.10)) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 51-4000 Metal Workers and Plastic Workers -----

hours_prod_welding_nr <- "51-4000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 1,
        shape2 = 35
    ) %>%
    mutate(
        Value_Hours = Value_Hours * 3 *
            runif(n = length(Value_Hours),
                  min = 0.90,
                  max =  1.10)) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 51-5100 Printing Workers -----

hours_prod_paint_nr <- "51-5100" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 1,
        shape2 = 35
    ) %>%
    mutate(
        Value_Hours = Value_Hours * 1 *
            runif(n = length(Value_Hours),
                  min = 0.90,
                  max =  1.10)) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 51-8000 Plant and System Operators -----

hours_prod_ops_nr <- "51-8000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>% mutate(
        NonrecurringOrRecurringID = "NONRECURRING")

# 51-9000 Other Production Occupations -----

hours_prod_other <- "51-9000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 2,
        shape2 = 35
    ) %>%
    mutate(
        Value_Hours = Value_Hours * 1 *
            runif(n = length(Value_Hours),
                  min = 0.90,
                  max =  1.10)) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 53-1000 Supervisors of Transportation and Material Moving Workers -----

hours_mat_supers_nr <- "53-1000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
    ) %>% mutate(
        NonrecurringOrRecurringID = "NONRECURRING")

# 53-3000 Motor Vehicle Operators -----

hours_mx_operators_nr <- "53-3000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 0,
        shape2 = 0
    ) %>%
    mutate(
        Value_Hours = Value_Hours * 50 *
            runif(n = length(Value_Hours),
                  min = 0.90,
                  max =  1.10)) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# 53-6000 Other Transportation Workers -----

hours_mat_other_nr <- "53-6000" %>%
    mutate_beta_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        shape1 = 0,
        shape2 = 0
    ) %>%
    mutate(
        Value_Hours = Value_Hours * 3 *
            runif(n = length(Value_Hours),
                  min = 0.90,
                  max =  1.10)) %>%
            mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
            mutate(Value_Hours = round(Value_Hours, 1))

# 53-7000 Material Moving Workers -----

hours_mat_movers_nr <- "53-7000" %>%
    mutate_normal_dist_hours(
        FunctionalCategories = functional_categories_table,
        ReportingCalendar = reporting_calendar_table,
        min = 0,
        max = 0
       ) %>%
    mutate(NonrecurringOrRecurringID = "NONRECURRING") %>%
    mutate(Value_Hours = round(Value_Hours, 1))

# Bind NONRECURRING hours into one tibble and remove 0 values -----

nonrecurring_hours <- bind_rows(
    hours_executive_nr,
    hours_sales_nr,
    hours_ops_mgmr_nr,
    hours_other_mgmt_nr,
    hours_biz_ops_nr,
    hours_finance_nr,
    hours_eng_cs_nr,
    hours_eng_math_nr,
    hours_eng_nr,
    hours_eng_tech_nr,
    hours_design_nr,
    hours_comm_nr,
    hours_comm_equ_nr,
    hours_ot_nr,
    hours_other_heath_nr,
    hours_sales_super_nr,
    hours_sales_field_nr,
    hours_sales_ws_nr,
    hours_sales_other_nr,
    hours_office_supers_nr,
    hours_finance_clerks_nr,
    hours_other_clerks_nr,
    hours_record_nr,
    hours_admin_assistant_nr,
    hours_other_office_nr,
    hours_const_supers_nr,
    hours_const_trade_nr,
    hours_const_support_nr,
    hours_const_other_nr,
    hours_mx_supers_nr,
    hours_mx_elec_nr,
    hours_mx_vehicle_nr,
    hours_mx_other_nr,
    hours_prod_supers_nr,
    hours_prod_assembly_nr,
    hours_prod_welding_nr,
    hours_prod_paint_nr,
    hours_prod_ops_nr,
    hours_prod_other,
    hours_mat_supers_nr,
    hours_mx_operators_nr,
    hours_mat_other_nr,
    hours_mat_movers_nr
) %>% filter(Value_Hours > 0.5,
             Value_Hours < Inf)

rm(
    hours_executive_nr,
    hours_sales_nr,
    hours_ops_mgmr_nr,
    hours_other_mgmt_nr,
    hours_biz_ops_nr,
    hours_finance_nr,
    hours_eng_cs_nr,
    hours_eng_math_nr,
    hours_eng_nr,
    hours_eng_tech_nr,
    hours_design_nr,
    hours_comm_nr,
    hours_comm_equ_nr,
    hours_ot_nr,
    hours_other_heath_nr,
    hours_sales_super_nr,
    hours_sales_field_nr,
    hours_sales_ws_nr,
    hours_sales_other_nr,
    hours_office_supers_nr,
    hours_finance_clerks_nr,
    hours_other_clerks_nr,
    hours_record_nr,
    hours_admin_assistant_nr,
    hours_other_office_nr,
    hours_const_supers_nr,
    hours_const_trade_nr,
    hours_const_support_nr,
    hours_const_other_nr,
    hours_mx_supers_nr,
    hours_mx_elec_nr,
    hours_mx_vehicle_nr,
    hours_mx_other_nr,
    hours_prod_supers_nr,
    hours_prod_assembly_nr,
    hours_prod_welding_nr,
    hours_prod_paint_nr,
    hours_prod_ops_nr,
    hours_prod_other,
    hours_mat_supers_nr,
    hours_mx_operators_nr,
    hours_mat_other_nr,
    hours_mat_movers_nr
)

# Join hourly_rate
nonrecurring_hours <- oes_data %>% select(occ_code, "hourly_rate" = h_mean) %>%
    mutate(hourly_rate = as.numeric(hourly_rate)) %>%
    left_join(nonrecurring_hours,
              .,
              by = c("ID" = "occ_code"))

# Mutate the constant hourly_rate with an inflation_rate
nonrecurring_hours <- inflation_table %>%
    select(StartDate, inflation_rate) %>%
    left_join(nonrecurring_hours,
              .,
              by = c("StartDate" = "StartDate")) %>%
    mutate(hourly_rate = hourly_rate * inflation_rate)

# Create the Value_Dollars column and drop the unneeded columns
nonrecurring_hours <- nonrecurring_hours %>%
    mutate("Value_Dollars" = round(Value_Hours * hourly_rate,2)) %>%
    select(-hourly_rate,
           -inflation_rate)
