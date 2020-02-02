
library(purrr)
library(tibble)

wbs_dictionary_definitions_table <- tibble(
    WBSElementID = wbs_table$ID,
    Text = map_chr(WBSElementID, stringi::stri_rand_lipsum, nparagraphs = 1)
)
