
# DOWNLOAD AND READ DATA ------------------------------------------------------
# We need a list of different jobs codes and titles to use as Functional
# Categories.  The BLS has just this type of thing which they use to perform
# their statistics/econometrics on (presumably).  We will take that list and use
# it for our own purposes.

library(tidyverse)
library(tidyxl)
library(unpivotr)

url <- "https://www.bls.gov/soc/2018/soc_2018_definitions.xlsx"
destfile <- "soc_2018_definitions.xlsx"
curl::curl_download(url, destfile)
soc_2018_definitions <- tidyxl::xlsx_cells(destfile)

# SAVE A COPY OF DATA ---------------------------------------------------------

curl::curl_download(url, here::here("data", paste0(Sys.Date(), "-soc_2018_definitions.xlsx")))

# CLEAN UP --------------------------------------------------------------------

unlink(destfile)
rm(url, destfile)

# MUNGE -----------------------------------------------------------------------

soc_defs <- soc_2018_definitions %>%
    dplyr::filter(row >= 8,                        # Remove header rows
                  is_blank != TRUE,                # Remove blanks
                  character != "") %>%             # REMOVE BLANKS
    unpivotr::behead(direction = "N", name = header) %>%
    dplyr::select(data_type, row, character, header) %>%
    unpivotr::spatter(header) %>%
    dplyr::filter(
        !is.na(`SOC Definition`)) %>%          # Remove anything without a def
    dplyr::select(# Fix column names and order
        occ_code  = `SOC Code`,
        occ_title = `SOC Title`,
        occ_group = `SOC Group`,
        occ_def   = `SOC Definition`)
