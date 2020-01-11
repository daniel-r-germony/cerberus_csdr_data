
library(tidyverse)
library(tidyxl)
library(unpivotr)

# DOWNLOAD AND READ DATA ------------------------------------------------------
# Go to each URL in "urls", download the file found at the URL and give it the
# name in "destfiles" and save it in the "FlexFile Three Part Template" folder.

urls <- c(
    "https://cade.osd.mil/content/cade/files/csdr/dids/current/Flex%20File%20-%20Alt%20Sub%20Template%20-%20Part%201%20-%20Metadata%20and%20Structures_Mar2019.xlsx",
    "https://cade.osd.mil/content/cade/files/csdr/dids/current/Flex%20File%20-%20Alt%20Sub%20Template%20-%20Part%202%20-%20Actual%20Cost-Hour%20Data_March%202019.xlsx",
    "https://cade.osd.mil/content/cade/files/csdr/dids/current/Flex%20File%20-%20Alt%20Sub%20Template%20-%20Part%203%20-%20Supplemental%20Data_March%202019.xlsx",
    "https://cade.osd.mil/content/cade/files/csdr/dids/current/QuantityReport_Template.xlsx")

fs::dir_create(here::here("data","FlexFile Three Part Template"))

destfiles <- c("FlexFile Excel Template Part 1 - Metadata & Structures - Mar 2019.xlsx",
               "FlexFile Excel Template Part 2 - Actual Cost-Hour Data - Mar 2019.xlsx",
               "FlexFile Excel Template Part 3 - Supplemental Data - Mar 2019.xlsx",
               "Quantity Excel Template - Mar 2019.xlsx")


walk2(urls, here::here("data","FlexFile Three Part Template", destfiles), download.file, mode = "wb")

ff_part1 <-
    tidyxl::xlsx_cells(
        here::here(
            "data",
            "FlexFile Three Part Template",
            "FlexFile Excel Template Part 1 - Metadata & Structures - Mar 2019.xlsx"
        )
    )

ff_part2 <-
    tidyxl::xlsx_cells(
        here::here(
            "data",
            "FlexFile Three Part Template",
            "FlexFile Excel Template Part 2 - Actual Cost-Hour Data - Mar 2019.xlsx"
        )
    )

ff_part3 <-
    tidyxl::xlsx_cells(
        here::here(
            "data",
            "FlexFile Three Part Template",
            "FlexFile Excel Template Part 3 - Supplemental Data - Mar 2019.xlsx"
        )
    )

ff_qty <-
    tidyxl::xlsx_cells(
        here::here(
            "data",
            "FlexFile Three Part Template",
            "Quantity Excel Template - Mar 2019.xlsx"
        )
    )
