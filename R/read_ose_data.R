
# DOWNLOAD AND READ DATA ------------------------------------------------------
# We need rates data for the labor categories we are using as Functional
# Categories.  Again, BLS has done the work to both create various labor
# categories and they have collected various statistics regarding pay for these
# labor categories.

library(tidyverse)

url <- "https://www.bls.gov/oes/special.requests/oesm18nat.zip"
temp <- tempfile()
download.file(url, temp)
unzip(temp)
field_descriptions <- tidyxl::xlsx_cells("oesm18nat/field_descriptions.xlsx")
national_M2018_dl <- tidyxl::xlsx_cells("oesm18nat/national_M2018_dl.xlsx")

# SAVE A COPY OF DATA ---------------------------------------------------------

curl::curl_download(url, here::here("data", paste0(Sys.Date(), "-oesm18nat.zip")))

# CLEAN UP --------------------------------------------------------------------

unlink(here::here("oesm18nat"), recursive = TRUE)
rm(temp, url)

# MUNGE -----------------------------------------------------------------------

# tidy "oesm18nat/field_descriptions.xlsx" ------------------------------------

oes_field_descriptions <- field_descriptions %>%
    dplyr::filter(row %in% 11:42) %>%
    unpivotr::behead(direction = "W", name = "oes_field") %>%
    dplyr::select(oes_field, "oes_field_description" = character)

oes_sentinel_descriptions <- field_descriptions %>%
    dplyr::filter(row %in% 45:48, is_blank != TRUE) %>%
    dplyr::select(character) %>%
    tidyr::separate(character, sep = "=", into = c("character", "definition")) %>%
    purrr::map_df(str_trim) %>%
    dplyr::select("sentinel_char" = character, "sentinel_def" = definition)

# tidy "oesm18nat/national_M2018_dl.xlsx" -------------------------------------

oes_data <- national_M2018_dl %>%
    unpivotr::isolate_sentinels(character, c(oes_sentinel_descriptions$sentinel_char)) %>%
    dplyr::select(row, col, data_type, character, numeric) %>%
    unpivotr::behead(direction = "N", name = "oes_field") %>%
    dplyr::select(-col) %>%
    unpivotr::spatter(oes_field) %>%
    dplyr::select(-row, OCC_CODE, OCC_TITLE, OCC_GROUP, dplyr::everything()) %>%
    dplyr::rename_all(str_to_lower)

oes_data_sentinels <- national_M2018_dl %>%
    dplyr::select(row, col, data_type, character) %>%
    unpivotr::behead(direction = "N", name = "oes_field") %>%
    unpivotr::behead(direction = "W", name = "OCC_CODE") %>%
    unpivotr::isolate_sentinels(character, c(oes_sentinel_descriptions$sentinel_char)) %>%
    dplyr::filter(!is.na(sentinel)) %>%
    dplyr::select(OCC_CODE, oes_field, oes_sentinel = sentinel) %>%
    dplyr::rename_all(str_to_lower) %>%
    dplyr::left_join(oes_sentinel_descriptions, by = c("oes_sentinel" = "sentinel_char"))
