
library(magrittr)
library(dplyr)

functional_categories_table <- oes_data %>%
    filter(occ_group == "minor") %>%
    select(ID = occ_code,
           Name = occ_title)

functional_categories_table <- functional_categories_table %>%
    mutate(detailed_standard_category_id = case_when(
ID == "11-1000" ~ "OTHER_OVERHEAD",	# Top Executives
ID == "11-2000" ~ "OTHER_OVERHEAD", # Advertising, Marketing, Promotions, Public Relations, and Sales Managers
ID == "11-3000" ~ "ENGINEERING_LABOR_OVERHEAD", # Operations Specialties Managers
ID == "11-9000" ~ "DIRECT_PROGRAM_MANAGEMENT_LABOR", # Other Management Occupations
ID == "13-1000" ~ "DIRECT_PROGRAM_MANAGEMENT_LABOR", # Business Operations Specialists
ID == "13-2000" ~ "DIRECT_PROGRAM_MANAGEMENT_LABOR", # Financial Specialists
ID == "15-1100" ~ "DIRECT_ENGINEERING_LABOR", # Computer Occupations
ID == "15-2000" ~ "DIRECT_PROGRAM_MANAGEMENT_LABOR", # Mathematical Science Occupations
ID == "17-1000" ~ NA_character_, # Architects, Surveyors, and Cartographers
ID == "17-2000" ~ "DIRECT_ENGINEERING_LABOR", # Engineers
ID == "17-3000" ~ "DIRECT_ENGINEERING_LABOR", # Drafters, Engineering Technicians, and Mapping Technicians
ID == "19-1000" ~ NA_character_, # Life Scientists
ID == "19-2000" ~ NA_character_, # Physical Scientists
ID == "19-3000" ~ NA_character_, # Social Scientists and Related Workers
ID == "19-4000" ~ NA_character_, # Life, Physical, and Social Science Technicians
ID == "21-1000" ~ NA_character_, # Counselors, Social Workers, and Other Community and Social Service Specialists
ID == "21-2000" ~ NA_character_, # Religious Workers
ID == "23-1000" ~ NA_character_, # Lawyers, Judges, and Related Workers
ID == "23-2000" ~ NA_character_, # Legal Support Workers
ID == "25-1000" ~ NA_character_, # Postsecondary Teachers
ID == "25-2000" ~ NA_character_, # Preschool, Primary, Secondary, and Special Education School Teachers
ID == "25-3000" ~ NA_character_, # Other Teachers and Instructors
ID == "25-4000" ~ NA_character_, # Librarians, Curators, and Archivists
ID == "25-9000" ~ NA_character_, # Other Education, Training, and Library Occupations
ID == "27-1000" ~ "DIRECT_SERVICES", # Art and Design Workers
ID == "27-2000" ~ NA_character_, # Entertainers and Performers, Sports and Related Workers
ID == "27-3000" ~ "DIRECT_SERVICES", # Media and Communication Workers
ID == "27-4000" ~ "DIRECT_SERVICES", # Media and Communication Equipment Workers
ID == "29-1000" ~ NA_character_, # Health Diagnosing and Treating Practitioners
ID == "29-2000" ~ NA_character_, # Health Technologists and Technicians
ID == "29-9000" ~ NA_character_, # Other Healthcare Practitioners and Technical Occupations
ID == "31-1000" ~ NA_character_, # Nursing, Psychiatric, and Home Health Aides
ID == "31-2000" ~ "DIRECT_OTHER_LABOR", # Occupational Therapy and Physical Therapist Assistants and Aides
ID == "31-9000" ~ "DIRECT_OTHER_LABOR", # Other Healthcare Support Occupations
ID == "33-1000" ~ NA_character_, # Supervisors of Protective Service Workers
ID == "33-2000" ~ NA_character_, # Fire Fighting and Prevention Workers
ID == "33-3000" ~ NA_character_, # Law Enforcement Workers
ID == "33-9000" ~ NA_character_, # Other Protective Service Workers
ID == "35-1000" ~ NA_character_, # Supervisors of Food Preparation and Serving Workers
ID == "35-2000" ~ NA_character_, # Cooks and Food Preparation Workers
ID == "35-3000" ~ NA_character_, # Food and Beverage Serving Workers
ID == "35-9000" ~ NA_character_, # Other Food Preparation and Serving Related Workers
ID == "37-1000" ~ NA_character_, # Supervisors of Building and Grounds Cleaning and Maintenance Workers
ID == "37-2000" ~ NA_character_, # Building Cleaning and Pest Control Workers
ID == "37-3000" ~ NA_character_, # Grounds Maintenance Workers
ID == "39-1000" ~ NA_character_, # Supervisors of Personal Care and Service Workers
ID == "39-2000" ~ NA_character_, # Animal Care and Service Workers
ID == "39-3000" ~ NA_character_, # Entertainment Attendants and Related Workers
ID == "39-4000" ~ NA_character_, # Funeral Service Workers
ID == "39-5000" ~ NA_character_, # Personal Appearance Workers
ID == "39-6000" ~ NA_character_, # Baggage Porters, Bellhops, and Concierges
ID == "39-7000" ~ NA_character_, # Tour and Travel Guides
ID == "39-9000" ~ NA_character_, # Other Personal Care and Service Workers
ID == "41-1000" ~ "GENERAL_AND_ADMINISTRATIVE", # Supervisors of Sales Workers
ID == "41-2000" ~ NA_character_, # Retail Sales Workers
ID == "41-3000" ~ "GENERAL_AND_ADMINISTRATIVE", # Sales Representatives, Services
ID == "41-4000" ~ "GENERAL_AND_ADMINISTRATIVE", # Sales Representatives, Wholesale and Manufacturing
ID == "41-9000" ~ "GENERAL_AND_ADMINISTRATIVE", # Other Sales and Related Workers
ID == "43-1000" ~ "GENERAL_AND_ADMINISTRATIVE", # Supervisors of Office and Administrative Support Workers
ID == "43-2000" ~ NA_character_, # Communications Equipment Operators
ID == "43-3000" ~ "GENERAL_AND_ADMINISTRATIVE", # Financial Clerks
ID == "43-4000" ~ "GENERAL_AND_ADMINISTRATIVE", # Information and Record Clerks
ID == "43-5000" ~ "MANUFACTURING_OPERATIONS_LABOR_OVERHEAD", # Material Recording, Scheduling, Dispatching, and Distributing Workers
ID == "43-6000" ~ "DIRECT_OTHER_LABOR", # Secretaries and Administrative Assistants
ID == "43-9000" ~ "DIRECT_OTHER_LABOR", # Other Office and Administrative Support Workers
ID == "45-1000" ~ NA_character_, # Supervisors of Farming, Fishing, and Forestry Workers
ID == "45-2000" ~ NA_character_, # Agricultural Workers
ID == "45-4000" ~ NA_character_, # Forest, Conservation, and Logging Workers
ID == "47-1000" ~ "DIRECT_TOOLING_AND_EQUIPMENT", # Supervisors of Construction and Extraction Workers
ID == "47-2000" ~ "DIRECT_TOOLING_AND_EQUIPMENT", # Construction Trades Workers
ID == "47-3000" ~ "DIRECT_TOOLING_AND_EQUIPMENT", # Helpers, Construction Trades
ID == "47-4000" ~ "DIRECT_MANUFACTURING_OTHER_LABOR", # Other Construction and Related Workers
ID == "47-5000" ~ NA_character_, # Extraction Workers
ID == "49-1000" ~ "MAINTENANCE_OPERATIONS_LABOR_OVERHEAD", # Supervisors of Installation, Maintenance, and Repair Workers
ID == "49-2000" ~ "DIRECT_MAINTENANCE_TOUCH_LABOR", # Electrical and Electronic Equipment Mechanics, Installers, and Repairers
ID == "49-3000" ~ "DIRECT_MAINTENANCE_TOUCH_LABOR", # Vehicle and Mobile Equipment Mechanics, Installers, and Repairers
ID == "49-9000" ~ "DIRECT_MAINTENANCE_OTHER_LABOR", # Other Installation, Maintenance, and Repair Occupations
ID == "51-1000" ~ "MANUFACTURING_OPERATIONS_LABOR_OVERHEAD", # Supervisors of Production Workers
ID == "51-2000" ~ "DIRECT_MANUFACTURING_TOUCH_LABOR", # Assemblers and Fabricators
ID == "51-3000" ~ NA_character_, # Food Processing Workers
ID == "51-4000" ~ "DIRECT_MANUFACTURING_TOOLING_LABOR", # Metal Workers and Plastic Workers
ID == "51-5100" ~ "DIRECT_MANUFACTURING_TOUCH_LABOR", # Printing Workers
ID == "51-6000" ~ NA_character_, # Textile, Apparel, and Furnishings Workers
ID == "51-7000" ~ NA_character_, # Woodworkers
ID == "51-8000" ~ "DIRECT_MANUFACTURING_SUPPORT_LABOR", # Plant and System Operators
ID == "51-9000" ~ "DIRECT_MANUFACTURING_OTHER_LABOR", # Other Production Occupations
ID == "53-1000" ~ "MATERIAL_OVERHEAD", # Supervisors of Transportation and Material Moving Workers
ID == "53-2000" ~ NA_character_, # Air Transportation Workers
ID == "53-3000" ~ "DIRECT_MAINTENANCE_SUPPORT_LABOR", # Motor Vehicle Operators
ID == "53-4000" ~ NA_character_, # Rail Transportation Workers
ID == "53-5000" ~ NA_character_, # Water Transportation Workers
ID == "53-6000" ~ "MATERIAL_OVERHEAD", # Other Transportation Workers
ID == "53-7000" ~ "MATERIAL_OVERHEAD", # Material Moving Workers
TRUE ~ NA_character_
    ))

functional_categories_table <- functional_categories_table %>%
    filter(!is.na(detailed_standard_category_id))

# Create a count of how many of the 'oes_data' catigories have been crosswalked
# to the 'detailed_standard_category_id'
detailed_standard_category_id %>%
    left_join({functional_categories_table %>% count(detailed_standard_category_id)},
              by = c("value" = "detailed_standard_category_id")) %>% print(n = Inf)
