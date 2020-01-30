
library(magrittr)
library(dplyr)
library(tibble)

report_metadata_table <- tribble(
                                        ~Field,                                ~Value,
                     "Security Classification",                        "UNCLASSIFIED",
                       "Proprietary Statement",                                    NA,
                                "Program Name", "Cerberus Autonomous Vehicle Program",
                       "Phase or Milestone ID",                            "MULTIPLE",
                       "Prime Mission Product",        "Cerberus Autonomous Vehicles",
                              "Commodity Type",               "Ground Vehicle System",
    "Reporting Organization/Organization Name",                    "Con-Tractor, LLC",
        "Reporting Organization/Division Name",         "Autonomous Vehicle Division",
            "Reporting Organization/CAGE Code",                                    NA,
      "Reporting Organization/Location/Street",                         "57th Street",
        "Reporting Organization/Location/City",                        "New New York",
       "Reporting Organization/Location/State",                                  "NY",
    "Reporting Organization/Location/Zip Code",                               "10019",
     "Reporting Organization/Location/Country",            "United States of America",
                        "Approved Plan Number",                             "Z-99-C1",
               "Approved Plan Revision Number",                                    NA,
                               "Customer Name",                                    NA,
                            "Contract Type ID",                            "MULTIPLE",
                              "Contract Price",                                    NA,
                            "Contract Ceiling",                                    NA,
                             "Contract Number",                    "W123D4-56-P-7890",
            "Period of Performance/Start Date",                                    NA,
              "Period of Performance/End Date",                                    NA,
                             "Report Cycle ID",                                    NA,
                     "Submission Event/Number",                                    NA,
                       "Submission Event/Name",                                    NA,
                "Submission Event/Is Wildcard",                               "FALSE",
                         "Resubmission Number",                                    NA,
                                "Report As Of",                                    NA,
                       "Point of Contact/Name",                         "Schmoe, Joe",
                 "Point of Contact/Department",         "Autonomous Vehicle Division",
           "Point of Contact/Telephone Number",                        "212-555-1234",
              "Point of Contact/Email Address",          "joe.schmoe@con-tractor.com",
                               "Date Prepared",                                    NA,
                            "Report Period ID",                                    NA
    )

ReportMetadata <- list(
    SecurityClassification = report_metadata_table %>%
        filter(Field == "Security Classification") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    ProprietaryStatement = report_metadata_table %>%
        filter(Field == "Proprietary Statement") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    ProgramName = report_metadata_table %>%
        filter(Field == "Program Name") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    PhaseOrMilestoneID = report_metadata_table %>%
        filter(Field == "Phase or Milestone ID") %>%
        select(Value) %>%
        pull(-1) %>%
        factor(levels = c("PRE_A",
                          "A",
                          "B",
                          "C_LRIP",
                          "C_FRP",
                          "O_AND_S",
                          "MULTIPLE"),
               ordered = TRUE),
    PrimeMissionProduct = report_metadata_table %>%
        filter(Field == "Prime Mission Product") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    CommodityType = report_metadata_table %>%
        filter(Field == "Commodity Type") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    ReportingOrganization_OrganizationName = report_metadata_table %>%
        filter(Field == "Reporting Organization/Organization Name") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    ReportingOrganization_DivisionName = report_metadata_table %>%
        filter(Field == "Reporting Organization/Division Name") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    ReportingOrganization_CageCode = report_metadata_table %>%
        filter(Field == "Reporting Organization/CAGE Code") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    ReportingOrganization_Location_Street = report_metadata_table %>%
        filter(Field == "Reporting Organization/Location/Street") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    ReportingOrganization_Location_City = report_metadata_table %>%
        filter(Field == "Reporting Organization/Location/City") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    ReportingOrganization_Location_State = report_metadata_table %>%
        filter(Field == "Reporting Organization/Location/State") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    ReportingOrganization_Location_ZipCode = report_metadata_table %>%
        filter(Field == "Reporting Organization/Location/Zip Code") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    ReportingOrganization_Location_Country = report_metadata_table %>%
        filter(Field == "Reporting Organization/Location/Country") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    ApprovedPlanNumber = report_metadata_table %>%
        filter(Field == "Approved Plan Number") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    ApprovedPlanRevisionNumber = report_metadata_table %>%
        filter(Field == "Approved Plan Revision Number") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    CustomerName = report_metadata_table %>%
        filter(Field == "Customer Name") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    ContractTypeID = report_metadata_table %>%
        filter(Field == "Contract Type ID") %>%
        select(Value) %>%
        pull(-1) %>%
        factor(levels = c("CS",
                          "CPAF",
                          "CPFF",
                          "CPIF",
                          "CPIF_PI",
                          "FFP",
                          "FPIF",
                          "FPIST",
                          "FPIST_PI",
                          "FPIFT_PI",
                          "FPAF",
                          "FP_EPA",
                          "FP_PPR",
                          "FCP_RPR",
                          "FFP_LOET",
                          "IDIQ",
                          "LC",
                          "TM",
                          "OTHER",
                          "MULTIPLE")),
    ContractPrice = report_metadata_table %>%
        filter(Field == "Contract Price") %>%
        select(Value) %>%
        pull(-1) %>%
        as.numeric(),
    ContractCeiling = report_metadata_table %>%
        filter(Field == "Contract Ceiling") %>%
        select(Value) %>%
        pull(-1) %>%
        as.numeric(),
    ContractNumber = report_metadata_table %>%
        filter(Field == "Contract Number") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    PeriodOfPerformance_StartDate = report_metadata_table %>%
        filter(Field == "Period of Performance/Start Date") %>%
        select(Value) %>%
        pull(-1) %>%
        lubridate::ymd(),
    PeriodOfPerformance_EndDate = report_metadata_table %>%
        filter(Field == "Period of Performance/End Date") %>%
        select(Value) %>%
        pull(-1) %>%
        lubridate::ymd(),
    ReportCycleID = report_metadata_table %>%
        filter(Field == "Report Cycle ID") %>%
        select(Value) %>%
        pull(-1) %>%
        factor(levels = c("INITIAL",
                          "INTERIM",
                          "FINAL"),
        ordered = TRUE),
    SubmissionEvent_Number = report_metadata_table %>%
        filter(Field == "Submission Event/Number") %>%
        select(Value) %>%
        pull(-1) %>%
        as.integer(),
    SubmissionEvent_Name = report_metadata_table %>%
        filter(Field == "Submission Event/Name") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    SubmissionEvent_IsWildcard = report_metadata_table %>%
        filter(Field == "Submission Event/Is Wildcard") %>%
        select(Value) %>%
        pull(-1) %>%
        as.logical(),
    ResubmissionNumber = report_metadata_table %>%
        filter(Field == "Resubmission Number") %>%
        select(Value) %>%
        pull(-1) %>%
        as.integer(),
    ReportAsOf = report_metadata_table %>%
        filter(Field == "Report As Of") %>%
        select(Value) %>%
        pull(-1) %>%
        lubridate::ymd(),
    PointOfContact_Name = report_metadata_table %>%
        filter(Field == "Point of Contact/Name") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    PointOfContact_Department = report_metadata_table %>%
        filter(Field == "Point of Contact/Department") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    PointOfContact_TelephoneNumber = report_metadata_table %>%
        filter(Field == "Point of Contact/Telephone Number") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    PointOfContact_EmailAddress = report_metadata_table %>%
        filter(Field == "Point of Contact/Email Address") %>%
        select(Value) %>%
        pull(-1) %>%
        as.character(),
    DatePrepared = report_metadata_table %>%
        filter(Field == "Date Prepared") %>%
        select(Value) %>%
        pull(-1) %>%
        lubridate::ymd(),
    ReportingPeriodID = report_metadata_table %>%
        filter(Field == "Report Period ID") %>%
        select(Value) %>%
        pull(-1) %>%
        as.integer()
)
