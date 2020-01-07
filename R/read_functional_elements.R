
# READ DATA -------------------------------------------------------------------
# We need a list of different jobs codes and titles to use as Functional Categories.  The BLS has just this type of thing which they use to perform their statistics/econometrics on (presumably).  We will take that list and use it for our own purposes.

url <- "https://www.bls.gov/soc/2018/soc_2018_definitions.xlsx"
destfile <- "soc_2018_definitions.xlsx"
curl::curl_download(url, destfile)
soc_2018_definitions <- tidyxl::xlsx_cells(destfile)

# SAVE A COPY OF DATA ---------------------------------------------------------
# We need a list of different jobs codes and titles to use as Functional Categories.  The BLS has just this type of thing which they use to perform their statistics/econometrics on (presumably).  We will take that list and use it for our own purposes.

curl::curl_download(url, here::here("data", paste0(Sys.Date(), "-soc_2018_definitions.xlsx")))

sample_fce <- soc_2018_definitions %>%
    dplyr::filter(row >= 8,                        # Remove header rows
                  is_blank != TRUE,                # Remove blanks
                  character != "") %>%             # REMOVE BLANKS
    unpivotr::behead(direction = "N", name = header)

sample_fce %>%
    dplyr::select(data_type, row, character, header) %>%
    spatter(header) %>%
    dplyr::filter(
        !is.na(`SOC Definition`)) %>%          # Remove anything without a def
    dplyr::select(                             # Fix column names and order
                  fce_code  = `SOC Code`,
                  fce_nomen = `SOC Title`,
                  fce_def   = `SOC Definition`
                  )
