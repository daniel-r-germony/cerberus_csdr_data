
library(purrr)
library(tibble)
library(dplyr)

wbs_element_remarks <-
    expand.grid(OrderOrLotID = order_or_lots_table$ID,
                WBSElementID = wbs_table$ID) %>%
    arrange(OrderOrLotID) %>%
    as_tibble() %>%
    mutate(
        Text = map_chr(WBSElementID, stringi::stri_rand_lipsum, nparagraphs = 1)
)
