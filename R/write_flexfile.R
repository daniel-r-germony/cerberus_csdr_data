
#' Write a flexfile object to disk.
#'
#' Write a `flexfile` object to a Three-Part Excel Template or a JSON zip file.
#'
#' @param flexfile_to_write Required. The `filexfile` object to write.
#' @param output_path Required. The path to the output file. Do not include a
#'   filename or extension.
#' @param output_filename Optional. A user specified filename. Do not include
#'   file extension. Excel output will automatically append " - Part x.xlsx" to
#'   the user provided value. If no value is provided by the user, Excel files
#'   will be named "FlexFile Submission - Part x.xlxs".
#' @param output_format Required. Enter "Excel" to export into the three-part
#'   Excel template. !!!NOT CURRENTLY IMPLEMENTED Enter "JSON" to export into a
#'   JSON zip file.
#'
#' @return Returns noting to R put does write either Excel or JSON files to
#'   disk.
#' @export
#'
#' @examples
#' cerberus_submission_1 %>%
#'   write_flexfile(output_path = here::here("data"),
#'   output_filename = "Cerberus Submission 1",
#'   output_format = "Excel")
#'
#'   # Use default file names and defaults to Excel export
#'   cerberus_submission_1 %>%
#'   write_flexfile(output_path = here::here("data")
write_flexfile <- function(flexfile_to_write,
                           output_path,
                           output_filename = "",
                           output_format = "Excel") {

    # COPY THREE PART TEMPLATE ################################################
    # If Excel output is selected, create copies of the three part template,
    # name the new files the user provided filename or utilize a default.
    # #########################################################################

    if (output_format != "Excel") {
        stop('`output_format` currently only supports Excel.\nUse `output_format = "Excel"`',
             call. = FALSE
        )
    }

    part_1_filename <- dplyr::if_else(
        condition = output_filename == "",
        true = "FlexFile Submission - Part 1.xlsx",
        false = paste0(output_filename, " - Part 1.xlsx")
    )

    part_1_output_path <- paste0(output_path,
                                 "/",
                                 part_1_filename)

    part_2_filename <- dplyr::if_else(
        condition = output_filename == "",
        true = "FlexFile Submission - Part 2.xlsx",
        false = paste0(output_filename, " - Part 2.xlsx")
    )

    part_2_output_path <- paste0(output_path,
                                 "/",
                                 part_2_filename)

    part_3_filename <- dplyr::if_else(
        condition = output_filename == "",
        true = "FlexFile Submission - Part 3.xlsx",
        false = paste0(output_filename, " - Part 3.xlsx")
    )

    part_3_output_path <- paste0(output_path,
                                 "/",
                                 part_3_filename)

    # Create copy of part one
    # (note `invisible` stops `file.copy` from returning `TRUE` to the Console.)
    invisible(file.copy(
        from = here::here(
            "data",
            "FlexFile Three Part Template",
            "FlexFile Excel Template Part 1 - Metadata & Structures - Mar 2019.xlsx"
        ),
        to = part_1_output_path,
        overwrite = TRUE
    ))

    # Create copy of part two
    invisible(file.copy(
        from = here::here(
            "data",
            "FlexFile Three Part Template",
            "FlexFile Excel Template Part 2 - Actual Cost-Hour Data - Mar 2019.xlsx"
        ),
        to = part_2_output_path,
        overwrite = TRUE
    ))

    # Create copy of part three
    invisible(file.copy(
        from = here::here(
            "data",
            "FlexFile Three Part Template",
            "FlexFile Excel Template Part 3 - Supplemental Data - Mar 2019.xlsx"
        ),
        to = part_3_output_path,
        overwrite = TRUE
    ))

    # WRITE FLEXFILE DATA TO COPIED THREE PART TEMPLATE #######################
    # Use {openxlsx} to write the flexfile object data into the Excel files
    # #########################################################################

    part1 <- openxlsx::loadWorkbook(file = part_1_output_path)

    openxlsx::writeData(part1, sheet = "Report Configuration",
                        x = flexfile_to_write$ReportConfiguration %>%
                            enframe() %>%
                            unnest(value) %>%
                            select(value),
                        startCol = 2,
                        startRow = 3,
                        colNames = FALSE)

    # openxlsx::writeData(part1, sheet = "Report Metadata",
    #                     x = flexfile_to_write$ReportMetadata %>%
    #                         enframe() %>%
    #                         unnest(value) %>%
    #                         select(value),
    #                     startCol = 1,
    #                     startRow = 3,
    #                     colNames = FALSE)

    openxlsx::writeData(part1, sheet = "CLINs",
                        x = flexfile_to_write$CLINs,
                        startCol = 1,
                        startRow = 3,
                        colNames = FALSE)

    openxlsx::writeData(part1, sheet = "Orders or Lots",
                        x = flexfile_to_write$OrdersOrLots,
                        startCol = 1,
                        startRow = 3,
                        colNames = FALSE)

    openxlsx::writeData(part1, sheet = "End Items",
                        x = flexfile_to_write$EndItems,
                        startCol = 1,
                        startRow = 3,
                        colNames = FALSE)

    openxlsx::writeData(part1, sheet = "Work Breakdown Structure",
                        x = flexfile_to_write$WBS,
                        startCol = 1,
                        startRow = 3,
                        colNames = FALSE)

    openxlsx::writeData(part1, sheet = "Accounts",
                        x = flexfile_to_write$Accounts,
                        startCol = 1,
                        startRow = 3,
                        colNames = FALSE)

    openxlsx::writeData(part1, sheet = "Functional Categories",
                        x = flexfile_to_write$FunctionalCategories,
                        startCol = 1,
                        startRow = 3,
                        colNames = FALSE)

    openxlsx::writeData(part1, sheet = "Functional Overhead Categories",
                        x = flexfile_to_write$FunctionalOverheadCategories,
                        startCol = 1,
                        startRow = 3,
                        colNames = FALSE)

    openxlsx::writeData(part1, sheet = "Units or Sublots",
                        x = flexfile_to_write$UnitsOrSublots,
                        startCol = 1,
                        startRow = 3,
                        colNames = FALSE)

    openxlsx::writeData(part1, sheet = "Reporting Calendar",
                        x = flexfile_to_write$ReportingCalendar,
                        startCol = 1,
                        startRow = 3,
                        colNames = FALSE)

    openxlsx::writeData(part1, sheet = "Summary Cost Data",
                        x = flexfile_to_write$SummaryCostData,
                        startCol = 1,
                        startRow = 3,
                        colNames = FALSE)

    openxlsx::writeData(part1, sheet = "Allocation Methods",
                        x = flexfile_to_write$AllocationMethods,
                        startCol = 1,
                        startRow = 3,
                        colNames = FALSE)

    openxlsx::writeData(part1, sheet = "Allocation Components",
                        x = flexfile_to_write$AllocationComponents,
                        startCol = 1,
                        startRow = 3,
                        colNames = FALSE)

    saveWorkbook(part1, file = part_1_output_path, overwrite = TRUE)

}

#  # proof of concept
# cerberus_submission_7 %>%
#   write_flexfile(output_path = here::here("data"),
#   output_filename = "Cerberus Submission 1",
#   output_format = "Excel")
# #
#
# part1 <- openxlsx::loadWorkbook(file = here::here("data", "Cerberus Submission 1 - Part 1.xlsx"))
# openxlsx::writeData(part1, sheet = "CLINs", x = cerberus_submission_1$CLINs, startCol = 1, startRow = 3, colNames = FALSE)
# saveWorkbook(part1, file = here::here("data", "Cerberus Submission 1 - Part 1.xlsx"), overwrite = TRUE)

