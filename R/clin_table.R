
library(magrittr)
library(dplyr)
library(tibble)
library(stringr)

clin_table <- tibble(
    ID = factor(c("0001", "0002", "0003", "0004", "0005", "0006",
                  "1001", "1002", "1003", "1004", "1005", "1006",
                  "2001", "2002", "2003", "2004", "2005", "2006",
                  "3001", "3002", "3003", "3004", "3005", "3006",
                  "4001", "4002", "4003", "4004", "4005", "4006"
)))

clin_table <- clin_table %>%
    mutate(Name = case_when(
        str_ends(ID, "01") ~ "General Purpose Variant",
        str_ends(ID, "02") ~ "Command & Control Variant",
        str_ends(ID, "03") ~ "Heavy Weapons Variant",
        str_ends(ID, "04") ~ "Kits",
        str_ends(ID, "05") ~ "Test & Evaluation Support",
        str_ends(ID, "06") ~ "Systems Engineering & Systems Engineering",
        TRUE ~ NA_character_
        )
    )

clin_table <- clin_table %>%
    mutate(ContractTypeID = case_when(
        str_ends(ID, "01") ~ "FFP",
        str_ends(ID, "02") ~ "FFP",
        str_ends(ID, "03") ~ "FFP",
        str_ends(ID, "04") ~ "FFP",
        str_ends(ID, "05") ~ "CPFF",
        str_ends(ID, "06") ~ "CPFF",
        TRUE ~ NA_character_
        )
    )

clin_table_plus <- clin_table %>%
    mutate(OrderOrLotID = case_when(
        ID %>% str_starts("0") ~ order_or_lots_table$ID[1],
        ID %>% str_starts("1") ~ order_or_lots_table$ID[2],
        ID %>% str_starts("2") ~ order_or_lots_table$ID[3],
        ID %>% str_starts("3") ~ order_or_lots_table$ID[4],
        ID %>% str_starts("4") ~ order_or_lots_table$ID[5],
        TRUE ~ NA_character_
    )) %>%
    mutate(EndItemID = case_when(
        Name == "General Purpose Variant" ~ "Cerberus Tactical Vehicle - General Purpose Variant",
        Name == "Command & Control Variant" ~ "erberus Tactical Vehicle - Command & Control Variant",
        Name == "Heavy Weapons Variant" ~ "Cerberus Tactical Vehicle - Heavy Weapons Variant",
        Name == "Test & Evaluation Support" ~ "Common",
        Name == "Systems Engineering & Systems Engineering" ~ "Common",
        TRUE ~ NA_character_
    ))
