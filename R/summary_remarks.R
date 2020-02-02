
library(purrr)
library(tibble)
library(stringi)

summary_remarks <- tibble(
    OrderOrLotID = order_or_lots_table$ID,
    Text = map_chr(OrderOrLotID, stringi::stri_rand_lipsum, nparagraphs = 1)
)
