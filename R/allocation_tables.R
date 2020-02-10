
allocation_methods_table <- tibble::tribble(
    ~ID, ~AllocationMethodTypeID, ~IsUnitOrSublotAllocationMethod, ~Name,
    1,   "PERCENT",               TRUE,                            "General Purpose Variant Allocation",
    2,   "PERCENT",               TRUE,                            "Command & Control Variant Allocation",
    3,   "PERCENT",               TRUE,                            "Heavy Weapons Variant Allocation"
)

gp_wbs_distro <- tibble::tribble(
    ~WBSElementID,                                   ~WBSElementName, ~PURCHASED_PARTS, ~PURCHASED_EQUIPMENT, ~RAW_MATERIALS, ~INTERCOMPANY_WORK_ORDERS, ~DIRECT_REPORTING_SUBCONTRACTOR,
    "1.1.1.1",       "CAV Integration, Assembly, Test, and Checkout",                0,                    0,            0.1,                         0,                               0,
    "1.1.1.2",                                 "Hull/Frame/Body/Cab",              0.1,                    0,            0.4,                         0,                               0,
    "1.1.1.3",                                "System Survivability",              0.1,                 0.15,            0.1,                         0,                               0,
    "1.1.1.4",                                     "Turret Assembly",                0,                    0,              0,                         0,                               0,
    "1.1.1.5",                                 "Suspension/Steering",             0.25,                  0.2,           0.15,                         0,                               0,
    "1.1.1.6",                                 "Vehicle Electronics",             0.15,                  0.1,            0.1,                         0,                               0,
    "1.1.1.7.1.1.1",                                  "Engine Block",              0.2,                  0.1,           0.05,                         0,                               0,
    "1.1.1.7.1.1.2",                                  "Turbocharger",             0.05,                    0,              0,                         0,                               0,
    "1.1.1.7.1.1.3",                          "Other Dressed Engine",                0,                    0,              0,                         0,                               0,
    "1.1.1.7.1.2",                                    "Transmission",                0,                 0.15,           0.05,                         0,                               0,
    "1.1.1.7.2",                                       "Drive Train",                0,                 0.05,              0,                         0,                               0,
    "1.1.1.8",                                "Auxiliary Automotive",                0,                    0,           0.05,                         0,                               0,
    "1.1.1.9",                                        "Fire Control",                0,                    0,              0,                         0,                               0,
    "1.1.1.10",                                           "Armament",                0,                    0,              0,                         0,                               0,
    "1.1.1.11",                      "Automatic Ammunition Handling",                0,                    0,              0,                         0,                               0,
    "1.1.1.12",             "Navigation and Remote Piloting Systems",             0.15,                  0.2,              0,                         0,                               0,
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
    "1.1.1.1", "CAV Integration, Assembly, Test, and Checkout",                0,                    0,            0.1,                         0,                               0,
    "1.1.1.2",                           "Hull/Frame/Body/Cab",              0.1,                    0,            0.4,                         0,                               0,
    "1.1.1.3",                          "System Survivability",              0.1,                 0.15,            0.1,                         0,                               0,
    "1.1.1.4",                               "Turret Assembly",                0,                    0,              0,                         0,                               0,
    "1.1.1.5",                           "Suspension/Steering",             0.25,                  0.2,           0.15,                         0,                               0,
    "1.1.1.6",                           "Vehicle Electronics",             0.15,                  0.1,            0.1,                         0,                               0,
    "1.1.1.7.1.1.1",                            "Engine Block",              0.2,                  0.1,           0.05,                         0,                               0,
    "1.1.1.7.1.1.2",                            "Turbocharger",             0.05,                    0,              0,                         0,                               0,
    "1.1.1.7.1.1.3",                    "Other Dressed Engine",                0,                    0,              0,                         0,                               0,
    "1.1.1.7.1.2",                              "Transmission",                0,                 0.15,           0.05,                         0,                               0,
    "1.1.1.7.2",                                 "Drive Train",                0,                 0.05,              0,                         0,                               0,
    "1.1.1.8",                          "Auxiliary Automotive",                0,                    0,           0.05,                       0.2,                               0,
    "1.1.1.9",                                  "Fire Control",                0,                    0,              0,                         0,                               0,
    "1.1.1.10",                                     "Armament",                0,                    0,              0,                         0,                               0,
    "1.1.1.11",                "Automatic Ammunition Handling",                0,                    0,              0,                         0,                               0,
    "1.1.1.12",       "Navigation and Remote Piloting Systems",             0.15,                  0.2,              0,                         0,                               0,
    "1.1.1.13",                            "Special Equipment",                0,                    0,              0,                       0.3,                               0,
    "1.1.1.14",                               "Communications",                0,                 0.05,              0,                       0.5,                               0,
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
    "1.1.1.1", "CAV Integration, Assembly, Test, and Checkout",                0,                    0,            0.1,                         0,                               0,
    "1.1.1.2",                           "Hull/Frame/Body/Cab",              0.1,                    0,            0.4,                         0,                               0,
    "1.1.1.3",                          "System Survivability",              0.1,                 0.15,            0.1,                         0,                               0,
    "1.1.1.4",                               "Turret Assembly",                0,                    0,              0,                         0,                               0,
    "1.1.1.5",                           "Suspension/Steering",             0.25,                  0.2,           0.15,                         0,                               0,
    "1.1.1.6",                           "Vehicle Electronics",             0.15,                  0.1,            0.1,                         0,                               0,
    "1.1.1.7.1.1.1",                            "Engine Block",              0.2,                  0.1,           0.05,                         0,                               0,
    "1.1.1.7.1.1.2",                            "Turbocharger",             0.05,                    0,              0,                         0,                               0,
    "1.1.1.7.1.1.3",                    "Other Dressed Engine",                0,                    0,              0,                         0,                               0,
    "1.1.1.7.1.2",                              "Transmission",                0,                 0.15,           0.05,                         0,                               0,
    "1.1.1.7.2",                                 "Drive Train",                0,                 0.05,              0,                         0,                               0,
    "1.1.1.8",                          "Auxiliary Automotive",                0,                    0,           0.05,                         0,                               0,
    "1.1.1.9",                                  "Fire Control",                0,                    0,              0,                       0.4,                               0,
    "1.1.1.10",                                     "Armament",                0,                    0,              0,                      0.15,                               1,
    "1.1.1.11",                "Automatic Ammunition Handling",                0,                    0,              0,                      0.35,                               0,
    "1.1.1.12",       "Navigation and Remote Piloting Systems",             0.15,                  0.2,              0,                         0,                               0,
    "1.1.1.13",                            "Special Equipment",                0,                    0,              0,                       0.1,                               0,
    "1.1.1.14",                               "Communications",                0,                 0.05,              0,                         0,                               0,
    "1.1.1.15",                         "CAV Software Release",                0,                    0,              0,                         0,                               0,
    "1.1.1.16",                          "Other CAV Subsystem",                0,                    0,              0,                         0,                               0
                     ) %>%
    pivot_longer(cols = -c(1:2),
                 names_to = "DetailedStandardCategoryID",
                 values_to = "PercentValue") %>%
    filter(PercentValue > 0) %>%
    mutate(EndItemID = "4_CTV_HW")

# allocation_components_table <- tibble::tribble(
#     ~AllocationMethodID, ~OrderOrLotID, ~EndItemID, ~WBSElementID, ~UnitOrSublotID, ~PercentValue,
#
# )

