
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
    } else {

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

    # PART 1 ------------------------------------------------------------------

    part1 <- openxlsx::loadWorkbook(file = part_1_output_path)

    openxlsx::writeData(part1, sheet = "Report Configuration",
                        x = flexfile_to_write$ReportConfiguration %>%
                            enframe() %>%
                            unnest(value) %>%
                            select(value),
                        startCol = 2,
                        startRow = 3,
                        colNames = FALSE)

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$SecurityClassification,
                        xy = c("B", 3)) # Security Classification

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$ProprietaryStatement,
                        xy = c("B", 4))

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$ProgramName,
                        xy = c("B", 5))	# Program Name

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$PhaseOrMilestoneID,
                        xy = c("B", 6))	# Phase or Milestone ID

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$PrimeMissionProduct,
                        xy = c("B", 7))	# Prime Mission Product

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$CommodityType,
                        xy = c("B", 8))	# Commodity Type

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$ReportingOrganization_OrganizationName,
                        xy = c("B", 9))	# Reporting Organization/Organization Name

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$ReportingOrganization_DivisionName,
                        xy = c("B", 10)) # Reporting Organization/Division Name

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$ReportingOrganization_CageCode,
                        xy = c("B", 11)) # Reporting Organization/CAGE Code

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$ReportingOrganization_Location_Street,
                        xy = c("B", 12)) # Reporting Organization/Location/Street

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$ReportingOrganization_Location_City,
                        xy = c("B", 13)) # Reporting Organization/Location/City

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$ReportingOrganization_Location_State,
                        xy = c("B", 14)) # Reporting Organization/Location/State

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$ReportingOrganization_Location_ZipCode,
                        xy = c("B", 15)) # Reporting Organization/Location/Zip Code

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$ReportingOrganization_Location_Country,
                        xy = c("B", 16)) # Reporting Organization/Location/Country

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$ApprovedPlanNumber,
                        xy = c("B", 17)) # Approved Plan Number

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$ApprovedPlanRevisionNumber,
                        xy = c("B", 18)) # Approved Plan Revision Number

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$CustomerName,
                        xy = c("B", 19)) # Customer Name

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$ContractTypeID,
                        xy = c("B", 20)) # Contract Type ID

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$ContractPrice,
                        xy = c("B", 21)) # Contract Price

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$ContractCeiling,
                        xy = c("B", 22)) # Contract Ceiling

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$ContractNumber,
                        xy = c("B", 23)) # Contract Number

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$PeriodOfPerformance_StartDate,
                        xy = c("B", 24)) # Period of Performance/Start Date

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$PeriodOfPerformance_EndDate,
                        xy = c("B", 25)) # Period of Performance/End Date

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$ReportCycleID,
                        xy = c("B", 26)) # Report Cycle ID

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$SubmissionEvent_Number,
                        xy = c("B", 27)) # Submission Event/Number

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$SubmissionEvent_Name,
                        xy = c("B", 28)) # Submission Event/Name

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadataSubmissionEvent_IsWildcard,
                        xy = c("B", 29)) # Submission Event/Is Wildcard

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$ResubmissionNumber,
                        xy = c("B", 30)) # Resubmission Number

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$ReportAsOf,
                        xy = c("B", 31)) # Report As Of

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$PointOfContact_Name,
                        xy = c("B", 32)) # Point of Contact/Name

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$PointOfContact_Department,
                        xy = c("B", 33)) # Point of Contact/Department

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$PointOfContact_TelephoneNumber,
                        xy = c("B", 34)) # Point of Contact/Telephone Number

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$PointOfContact_EmailAddress,
                        xy = c("B", 35)) # Point of Contact/Email Address

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$DatePrepared,
                        xy = c("B", 36)) # Date Prepared

    openxlsx::writeData(part1, sheet = "Report Metadata",
                        x = flexfile_to_write$ReportMetadata$ReportingPeriodID,
                        xy = c("B", 37)) # Report Period ID

    openxlsx::writeData(part1, sheet = "Orders or Lots",
                        x = flexfile_to_write$OrdersOrLots,
                        startCol = 1,
                        startRow = 3,
                        colNames = FALSE)

    openxlsx::writeData(part1, sheet = "CLINs",
                        x = flexfile_to_write$CLINs,
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

    rm(part1)

    # PART 2 ------------------------------------------------------------------

    part2 <- openxlsx::loadWorkbook(file = part_2_output_path)

    openxlsx::writeData(part2, sheet = "Actual Cost-Hour Data",
                        x = flexfile_to_write$ActualCostHourData,
                        startCol = 1,
                        startRow = 3,
                        colNames = FALSE)

    saveWorkbook(part2, file = part_2_output_path, overwrite = TRUE)

    rm(part2)

    # PART 3 ------------------------------------------------------------------

    part3 <- openxlsx::loadWorkbook(file = part_3_output_path)

    openxlsx::writeData(part3, sheet = "FAC Cost-Hour Data",
                        x = flexfile_to_write$ForecastAtCompletionCostHourData,
                        startCol = 1,
                        startRow = 3,
                        colNames = FALSE)

    openxlsx::writeData(part3, sheet = "Summary Remarks",
                        x = flexfile_to_write$SummaryRemarks,
                        startCol = 1,
                        startRow = 3,
                        colNames = FALSE)

    openxlsx::writeData(part3, sheet = "WBS Element Remarks",
                        x = flexfile_to_write$WBSElementRemark,
                        startCol = 1,
                        startRow = 3,
                        colNames = FALSE)

    openxlsx::writeData(part3, sheet = "WBS Dictionary Definitions",
                        x = flexfile_to_write$WBSDictionaryDefinitions,
                        startCol = 1,
                        startRow = 3,
                        colNames = FALSE)

    openxlsx::writeData(part3, sheet = "Cost-Hour Tag Definitions",
                        x = flexfile_to_write$CostHourTagDefinitions,
                        startCol = 1,
                        startRow = 3,
                        colNames = FALSE)

    saveWorkbook(part3, file = part_3_output_path, overwrite = TRUE)

    rm(part3)

}}
