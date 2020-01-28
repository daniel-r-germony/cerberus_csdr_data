
# DOWNLOAD AND READ DATA ------------------------------------------------------
# We need a list of different jobs codes and titles to use as Functional
# Categories.  The BLS has just this type of thing which they use to perform
# their statistics/econometrics on (presumably).  We will take that list and use
# it for our own purposes.

library(tidyverse)
library(tidyxl)
library(unpivotr)

destfile <- "soc_2018_definitions.xlsx"
url <- "https://www.bls.gov/soc/2018/soc_2018_definitions.xlsx"

if (utils::menu(c("Use the local copy.", "Replace local copy with new download."),
                title = "Do you want to use a local copy of the BLS data?") == 2) {

    curl::curl_download(url, destfile)

    # SAVE A COPY OF DATA ---------------------------------------------------------
    curl::curl_download(url, here::here("data","soc_2018_definitions.xlsx"))

}

soc_2018_definitions <- tidyxl::xlsx_cells(here::here("data",destfile))

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
