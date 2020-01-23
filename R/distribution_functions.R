
mutate_norm_dist <- function(df, names_to = "name", min = 0, max = 1, ...) {
    df %>% mutate(!!names_to := runif(nrow(df), min = min, max = max))
}

mutate_beta_dist <- function(df, names_to = "name", shape1, shape2, ...) {
    df %>% mutate(percent_complete = row_number() / max(row_number())) %>%
    mutate(!!names_to := stats::dbeta(x = percent_complete, shape1 = shape1, shape2 = shape2)) %>%
        select(-percent_complete)
}


# proof of concept reminder -----------------------------------------------

mutate_hour_values <- function(..., df) {
    df %>% mutate("Value_Hours" = runif(nrow(df), 0, 100))
}

functional_categories_table %>%
    filter(!is.na(detailed_standard_category_id)) %>%
    mutate("Hour_Values" = map(.[1], mutate_hour_values, df = reporting_calendar_table)) %>%
    unnest(c(Hour_Values), names_sep = "_")

