#' Government in office on a date
#'
#' @param date A date or `"YYYY-MM-DD"` string (a single value).
#' @return A tibble with the government in office on `date` (zero rows if
#'   none) and a list column `parties` with its parties.
#' @export
#' @examples
#' parl_government_on("1934-01-05")
parl_government_on <- function(date) {
  date <- parl_as_date(date, "date")
  if (length(date) != 1L) cli::cli_abort("{.arg date} must be a single date.")
  g <- parl_ref$governments
  g <- g[g$data_inici <= date & g$data_fi >= date, ]
  g$parties <- strsplit(as.character(ifelse(is.na(g$partits), "", g$partits)), "|", fixed = TRUE)
  g
}

#' Add government/opposition columns
#'
#' Adds `govern` and `es_govern` to any data frame with a `data` (date) and a
#' `party_data` column, using [parl_governments()]. `es_govern` is `NA` when
#' `party_data` is missing or there is no statutory government in office.
#'
#' @param data A data frame with columns `data` and `party_data`.
#' @return `data` with the columns `govern` and `es_govern` added (or replaced).
#' @export
#' @examples
#' x <- data.frame(data = as.Date(c("1933-05-02", "1936-06-03")), party_data = c("LR", "ERC"))
#' parl_add_government(x)
parl_add_government <- function(data) {
  if (!all(c("data", "party_data") %in% names(data))) {
    cli::cli_abort("{.arg data} must have the columns {.field data} and {.field party_data}.")
  }
  g <- parl_ref$governments
  dates <- parl_as_date(data$data, "data$data")
  idx <- vapply(dates, function(d) {
    i <- which(g$data_inici <= d & g$data_fi >= d)
    if (length(i)) i[1] else NA_integer_
  }, integer(1))
  parts <- strsplit(ifelse(is.na(g$partits), "", g$partits), "|", fixed = TRUE)
  data$govern <- g$govern[idx]
  no_gov <- is.na(idx) | is.na(g$partits[idx])
  data$es_govern <- ifelse(
    is.na(data$party_data) | no_gov, NA,
    mapply(function(p, i) p %in% parts[[i]], data$party_data, ifelse(is.na(idx), 1L, idx))
  )
  data
}
