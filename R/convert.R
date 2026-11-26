#' Convert speeches to a quanteda corpus
#'
#' @param data A tibble with a `text` column, as returned by [parl_speeches()]
#'   or [parl_search()].
#' @return A `quanteda` corpus. Document names are `sessio_ordre`; the other
#'   columns become document variables.
#' @export
#' @examples
#' \dontrun{
#' corp <- parl_as_corpus(parl_speeches(year = 1936))
#' }
parl_as_corpus <- function(data) {
  parl_need("quanteda")
  parl_need_text(data)
  df <- as.data.frame(data)
  df$doc_id <- paste(df$sessio, df$ordre, sep = "_")
  quanteda::corpus(df, text_field = "text", docid_field = "doc_id")
}

#' Tokenise speeches with tidytext
#'
#' @inheritParams parl_as_corpus
#' @param token,... Passed to [tidytext::unnest_tokens()].
#' @return A tibble with one row per token and the speech metadata.
#' @export
#' @examples
#' \dontrun{
#' parl_tidy(parl_speeches(year = 1938))
#' }
parl_tidy <- function(data, token = "words", ...) {
  parl_need("tidytext")
  parl_need_text(data)
  tidytext::unnest_tokens(data, output = "word", input = "text", token = token, ...)
}

parl_need <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort("Package {.pkg {pkg}} is needed: {.code install.packages(\"{pkg}\")}")
  }
}

parl_need_text <- function(data) {
  if (!"text" %in% names(data)) {
    cli::cli_abort("{.arg data} has no {.field text} column. Use {.code parl_speeches(text = TRUE)}.")
  }
}
