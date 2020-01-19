
# FlexFile Submission 1: Contract Award ---------------------------------------
cerberus_submission_1 <- flexfile

cerberus_submission_1$CLINs <- clin_table %>%
    filter(str_detect(ID, pattern = "^0"))

# FlexFile Submission 3: Delivery of all First Article Test Vehicles ----------
cerberus_submission_3 <- flexfile

cerberus_submission_3$CLINs <- clin_table %>%
    filter(str_detect(ID, pattern = "^0") |
           str_detect(ID, pattern = "^1"))

# FlexFile Submission 5: Complete LRIP Deliveries -----------------------------
cerberus_submission_5 <- flexfile

cerberus_submission_5$CLINs <- clin_table %>%
    filter(str_detect(ID, pattern = "^0") |
           str_detect(ID, pattern = "^1") |
           str_detect(ID, pattern = "^2"))

# FlexFile Submission 7: Deliveries/Contract Complete -------------------------
cerberus_submission_7 <- flexfile

cerberus_submission_7$CLINs <- clin_table %>%
    filter(str_detect(ID, pattern = "^0") |
           str_detect(ID, pattern = "^1") |
           str_detect(ID, pattern = "^2") |
           str_detect(ID, pattern = "^3") |
           str_detect(ID, pattern = "^4")
           )
