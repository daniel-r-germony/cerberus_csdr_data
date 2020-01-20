
library(tidyr)
library(dplyr)

extract_comment <- function(xlsx_object) {
    xlsx_object %>%
      select(sheet, character, comment) %>%
      filter(!is.na(comment)) %>%
      mutate(comment = str_remove_all(comment, pattern = "Supported values:")) %>%
      separate_rows(comment, sep = "\r\n") %>%
      filter(comment != "") %>%
      rename(catigory = character) %>%
      mutate(catigory = str_replace(catigory, pattern = "\\r\\n", replacement = " "))
}

ff_comments <- extract_comment(ff_part1) %>%
    bind_rows(., extract_comment(ff_part2)) %>%
    bind_rows(., extract_comment(ff_part3))

standard_category_id <- filter(ff_comments) %>%
    filter(catigory == "Standard Category ID") %>%
    select(value = comment) %>%
    distinct() %>%
    mutate(value = factor(value))

detailed_standard_category_id <- filter(ff_comments) %>%
    filter(catigory == "Detailed Standard Category ID") %>%
    select(value = comment) %>%
    distinct() %>%
    mutate(value = factor(value))
