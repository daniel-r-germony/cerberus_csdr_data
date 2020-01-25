
#' Mutate Normal Distribution Given FunctionalCategoryID and ReportingCalendar
#'
#' Create normally distributed, time phased data for a given
#' FunctionalCategoryID. The function is passed a copy of FunctionalCategory
#' table, the FunctionalCategoryID to create data for, and the ReportingCalendar
#' the data will be phased over along with normally distributed shape
#' paramaters.
#'
#' @param FunctionalCategories Required. A dataframe of avaiable
#'   FunctionalCategories.
#' @param FunctionalCategoryID Required. A single character string to subset
#'   from the FunctionalCategories table and generate random values against.
#' @param min The lower limit of the distribution. Must be finite.
#' @param max The upper limit of the distribution. Must be finite.
#' @param ReportingCalendar Required. The reporting calendar dataframe.
#' @param ...
#'
#' @return Returns the FunctionalCategory table, filtered to include only the
#'   provided FunctionalCategoryID, with the ReportingCalendar left_joined and a
#'   new "Value_Hours" column with the generated random values.
#'
#' @examples
#'
#' functional_categories_table %>%
#'   mutate_normal_dist_hours("11-1000",
#'     ReportingCalendar = reporting_calendar_table)
#'
#' functional_categories_table %>%
#'   mutate_normal_dist_hours("11-1000",
#'     ReportingCalendar = reporting_calendar_table,
#'     min = 100, max = 999)
#' @export
mutate_normal_dist_hours <- function(FunctionalCategories,
                FunctionalCategoryID,
                min = 0,
                max = 1,
                ReportingCalendar,
                ...
                ) {
    FunctionalCategories %>%
        filter(ID == FunctionalCategoryID) %>%
        mutate("Value_Hours" =
                   as.list(nest(
                       ReportingCalendar %>%
                           mutate("Value_Hours" = runif(
                               nrow(ReportingCalendar),
                               min = min,
                               max = max
                               )) %>%
                           rename(ReportingPeriodID = ID),
                       data = everything()))) %>%
        unnest(Value_Hours) %>%
        unnest(Value_Hours, names_repair = "universal")
}

#' Mutate Beta Distribution Given FunctionalCategoryID and ReportingCalendar
#'
#' Create beta distributed, time phased data for a given FunctionalCategoryID.
#' The function is passed a copy of FunctionalCategory table, the
#' FunctionalCategoryID to create data for, and the ReportingCalendar the data
#' will be phased over along with Beta distributing shape paramaters.
#'
#' @param FunctionalCategories Required. A dataframe of avaiable
#'   FunctionalCategories.
#' @param FunctionalCategoryID Required. A single character string to subset
#'   from the FunctionalCategories table and generate random values against.
#' @param shape1 Alpha paramater for the Beta distribution. Must be
#'   non-negative.
#' @param shape2 Beta paramater for the Beta distribution. Must be non-negative.
#' @param ReportingCalendar Required. The reporting calendar dataframe.
#' @param ...
#'
#' @return Returns the FunctionalCategory table, filtered to include only the
#'   provided FunctionalCategoryID, with the ReportingCalendar left_joined and a
#'   new "Value_Hours" column with the generated random values.
#'
#' @examples
#'
#' functional_categories_table %>%
#'   mutate_beta_dist_hours("11-1000",
#'     shape1 = 1, shape2 = 3,
#'     ReportingCalendar = reporting_calendar_table)
#'
#' functional_categories_table %>%
#'   mutate_beta_dist_hours("11-1000", 2, 5,
#'     ReportingCalendar = reporting_calendar_table)
#' @export
mutate_beta_dist_hours <- function(FunctionalCategories,
                            FunctionalCategoryID,
                            shape1,
                            shape2,
                            ReportingCalendar,
                            ...
                            ) {
    FunctionalCategories %>%
        filter(ID == FunctionalCategoryID) %>%
        mutate("Value_Hours" =
                   as.list(nest(
                       ReportingCalendar %>%
                           rename(ReportingPeriodID = ID) %>%
                           mutate(percent_complete = row_number() / max(row_number())) %>%
                           mutate("Value_Hours" = dbeta(
                               percent_complete,
                               shape1 = shape1,
                               shape2 = shape2
                           )),
                       data = everything()
                   ))) %>%
        unnest(Value_Hours) %>%
        unnest(Value_Hours, names_repair = "universal") %>%
        select(-percent_complete)
}
