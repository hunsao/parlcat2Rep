parl_check_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be TRUE or FALSE.")
  }
  invisible(x)
}

parl_check_year <- function(year) {
  if (!is.numeric(year) || anyNA(year)) cli::cli_abort("{.arg year} must be numeric.")
  year <- as.integer(year)
  available <- sort(unique(parl_meta$any))
  bad <- setdiff(year, available)
  if (length(bad)) {
    cli::cli_abort(c(
      "No sessions in {.val {bad}}.",
      "i" = "Available years: {paste(available, collapse = ', ')} (the Parliament was suspended in 1935)."
    ))
  }
  year
}

parl_as_date <- function(x, arg) {
  out <- tryCatch(as.Date(x), error = function(e) NULL)
  if (is.null(out) || anyNA(out)) {
    cli::cli_abort("{.arg {arg}} must be a date or a {.val YYYY-MM-DD} string.")
  }
  out
}

parl_check_values <- function(x, allowed, arg) {
  bad <- setdiff(x, allowed)
  if (length(bad)) {
    shown <- utils::head(sort(allowed), 15)
    cli::cli_abort(c(
      "Unknown {.arg {arg}}: {.val {bad}}.",
      "i" = "Valid values include: {paste(shown, collapse = ', ')}{if (length(allowed) > 15) ' ...' else ''}."
    ))
  }
  invisible(x)
}
