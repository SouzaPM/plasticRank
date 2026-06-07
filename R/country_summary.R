#' Summarize country-level indicators
#'
#' Aggregates observations to the country level and computes average
#' environmental and economic indicators for the selected year.
#'
#' @param year_select Year to filter (default = 2019)
#' @param data A cleaned data frame from clean_data(). If not provided,
#'   the function loads and cleans the data automatically.
#'
#' @return A summarized data frame with one row per country
#'
#' @export
country_summary <- function(year_select = 2019, data = NULL) {

  if (is.null(data)) {
    data <- clean_data(load_data())
  }

  if (!year_select %in% unique(data$year)) {
    stop("Year not found in dataset.")
  }

  data |>
    dplyr::filter(year == year_select) |>
    dplyr::group_by(country) |>
    dplyr::summarise(
      gdp_per_capita           = mean(gdp_per_capita, na.rm = TRUE),
      co2_per_capita           = mean(co2_per_capita, na.rm = TRUE),
      plastic_waste_per_capita = mean(plastic_waste_per_capita, na.rm = TRUE),
      population               = mean(population, na.rm = TRUE),
      .groups = "drop"
    )
}
