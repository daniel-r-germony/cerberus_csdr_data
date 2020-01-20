
library(magrittr)
library(dplyr)
library(tidyr)

functional_overhead_categories_table <- oes_data %>%
    select(occ_code, occ_group, occ_title) %>%
    filter(occ_group == "major") %>%
    separate(col = occ_code,
             into = c("ID", ".drop"),
             sep = "-") %>%
    select(ID, Name = occ_title) %>%
    mutate(Name = paste0(Name, " Overhead"))
