
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
write_flexfile <- function(flexfile_to_write,
                           output_path,
                           output_filename = "",
                           output_format = "Excel") {
    # Copy Three Part Template -----
    # If Excel output is selected, create copies of the three part template,
    # name the new files the user provided filename or utilize a default.

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

    part_2_filename <- dplyr::if_else(
        condition = output_filename == "",
        true = "FlexFile Submission - Part 2.xlsx",
        false = paste0(output_filename, " - Part 2.xlsx")
    )

    part_3_filename <- dplyr::if_else(
        condition = output_filename == "",
        true = "FlexFile Submission - Part 3.xlsx",
        false = paste0(output_filename, " - Part 3.xlsx")
    )

    # Create copy of part one
    # (note `invisible` stops `file.copy` from returning `TRUE` to the Console.)
    invisible(file.copy(
        from = here::here(
            "data",
            "FlexFile Three Part Template",
            "FlexFile Excel Template Part 1 - Metadata & Structures - Mar 2019.xlsx"
        ),
        to = paste0(output_path,
                    "/",
                    part_1_filename),
        overwrite = TRUE
    ))

    # Create copy of part two
    invisible(file.copy(
        from = here::here(
            "data",
            "FlexFile Three Part Template",
            "FlexFile Excel Template Part 2 - Actual Cost-Hour Data - Mar 2019.xlsx"
        ),
        to = paste0(output_path,
                    "/",
                    part_2_filename),
        overwrite = TRUE
    ))

    # Create copy of part three
    invisible(file.copy(
        from = here::here(
            "data",
            "FlexFile Three Part Template",
            "FlexFile Excel Template Part 3 - Supplemental Data - Mar 2019.xlsx"
        ),
        to = paste0(output_path,
                    "/",
                    part_3_filename),
        overwrite = TRUE
    ))
}
