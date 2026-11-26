#' Search the text of the speeches
#'
#' Returns the speeches whose text contains `pattern`.
#'
#' @param pattern Character. A word or phrase, or a regular expression when
#'   `regex = TRUE`.
#' @param ... Filters passed to [parl_speeches()] (`year`, `party`, `person`...).
#' @param regex Logical. Treat `pattern` as a regular expression (ICU syntax).
#' @param ignore_case Logical. Case-insensitive search (default `TRUE`).
#' @param ignore_accents Logical. Accent-insensitive search (default `TRUE`),
#'   useful with OCR and period spellings.
#'
#' @return A tibble as in [parl_speeches()], with an extra column `n_matches`.
#' @export
#' @examples
#' old <- parl_use_sample()
#' parl_search("Seu d'Urgell")
#' parl_use_sample(old)
parl_search <- function(pattern, ..., regex = FALSE, ignore_case = TRUE, ignore_accents = TRUE) {
  d <- parl_speeches(..., text = TRUE)
  n <- parl_count(d, pattern, regex, ignore_case, ignore_accents)
  d$n_matches <- n
  d[n > 0, ]
}

#' Keyword in context
#'
#' One row per occurrence of `pattern`, with the words before and after it.
#'
#' @inheritParams parl_search
#' @param window Integer. Number of words of context on each side.
#' @return A tibble with `sessio`, `ordre`, `data`, `nom_sencer`,
#'   `party_data`, `pre`, `match` and `post`.
#' @export
#' @examples
#' old <- parl_use_sample()
#' parl_kwic("Urgell", window = 5)
#' parl_use_sample(old)
parl_kwic <- function(pattern, window = 10, ..., regex = FALSE, ignore_case = TRUE,
                      ignore_accents = TRUE) {
  d <- parl_search(pattern, ..., regex = regex, ignore_case = ignore_case,
                   ignore_accents = ignore_accents)
  rx <- parl_regex(pattern, regex, ignore_accents)
  rows <- vector("list", nrow(d))
  for (i in seq_len(nrow(d))) {
    txt <- d$text[i]
    hay <- if (ignore_accents) parl_fold(txt, lower = FALSE) else txt
    shown <- txt
    loc <- stringi::stri_locate_all_regex(hay, rx, case_insensitive = ignore_case)[[1]]
    pre <- post <- mt <- character(nrow(loc))
    for (j in seq_len(nrow(loc))) {
      mt[j] <- substr(shown, loc[j, 1], loc[j, 2])
      pre[j] <- parl_last_words(substr(shown, 1, loc[j, 1] - 1), window)
      post[j] <- parl_first_words(substr(shown, loc[j, 2] + 1, nchar(shown)), window)
    }
    rows[[i]] <- data.frame(
      sessio = rep(d$sessio[i], nrow(loc)), ordre = d$ordre[i], data = d$data[i],
      nom_sencer = d$nom_sencer[i], party_data = d$party_data[i],
      pre = pre, match = mt, post = post, stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- data.frame(sessio = character(), ordre = integer(), data = as.Date(character()),
                      nom_sencer = character(), party_data = character(), pre = character(),
                      match = character(), post = character())
  }
  tibble::as_tibble(out)
}

#' Frequency of a term over time
#'
#' Counts the occurrences of `pattern` by month or year (and optionally by
#' party or by government/opposition) and divides them by the number of words
#' spoken in that period, giving a rate per million words. Periods without
#' sessions are kept with `paraules = 0` and `rate = NA` (for example, the
#' suspension of the Parliament, October 1934 - February 1936).
#'
#' @inheritParams parl_search
#' @param by `"month"` (default) or `"year"`.
#' @param group `"none"` (default), `"party"` (`party_data`) or `"side"`
#'   (government/opposition).
#' @param per Numeric. Rate denominator (default one million words).
#'
#' @return A tibble with `periode`, `grup`, `n_matches`, `n_speeches`
#'   (speeches with at least one match), `paraules` and `rate`.
#' @export
#' @examples
#' old <- parl_use_sample()
#' parl_trend("Govern", by = "year")
#' parl_use_sample(old)
parl_trend <- function(pattern, by = c("month", "year"), group = c("none", "party", "side"),
                       per = 1e6, ..., regex = FALSE, ignore_case = TRUE, ignore_accents = TRUE) {
  by <- match.arg(by)
  group <- match.arg(group)
  d <- parl_speeches(..., text = TRUE)
  n <- parl_count(d, pattern, regex, ignore_case, ignore_accents)

  d$periode <- if (by == "month") d$mes else as.character(d$any)
  d$grup <- switch(group,
    none = "all",
    party = ifelse(is.na(d$party_data), "(none)", d$party_data),
    side = ifelse(is.na(d$es_govern), "(none)", ifelse(d$es_govern, "government", "opposition"))
  )

  periods <- if (by == "month") {
    format(seq(as.Date(paste0(min(d$mes), "-01")), as.Date(paste0(max(d$mes), "-01")), by = "month"), "%Y-%m")
  } else {
    as.character(seq(min(d$any), max(d$any)))
  }
  grid <- expand.grid(periode = periods, grup = sort(unique(d$grup)), stringsAsFactors = FALSE)
  key <- paste(d$periode, d$grup)
  gkey <- paste(grid$periode, grid$grup)
  grid$n_matches <- as.numeric(tapply(n, factor(key, levels = gkey), sum))
  grid$n_speeches <- as.numeric(tapply(n > 0, factor(key, levels = gkey), sum))
  grid$paraules <- as.numeric(tapply(d$paraules, factor(key, levels = gkey), sum))
  grid[is.na(grid)] <- 0
  grid$rate <- ifelse(grid$paraules > 0, grid$n_matches / grid$paraules * per, NA_real_)
  out <- tibble::as_tibble(grid[order(grid$grup, grid$periode), ])
  attr(out, "by") <- by
  attr(out, "pattern") <- pattern
  out
}

#' Plot the frequency of a term over time
#'
#' @param trend A tibble returned by [parl_trend()].
#' @param title Plot title. Defaults to the searched pattern.
#' @return A `ggplot` object.
#' @export
#' @examples
#' old <- parl_use_sample()
#' parl_plot_trend(parl_trend("Govern", by = "year"))
#' parl_use_sample(old)
parl_plot_trend <- function(trend, title = NULL) {
  if (!all(c("periode", "grup", "rate") %in% names(trend))) {
    cli::cli_abort("{.arg trend} must be the result of {.fn parl_trend}.")
  }
  by <- attr(trend, "by") %||% "month"
  if (is.null(title)) title <- attr(trend, "pattern")
  trend$x <- if (by == "month") as.Date(paste0(trend$periode, "-01")) else as.integer(trend$periode)

  p <- ggplot2::ggplot(trend, ggplot2::aes(x = .data$x, y = .data$rate, colour = .data$grup))
  if (by == "month") {
    p <- p + ggplot2::annotate("rect", xmin = as.Date("1934-10-01"), xmax = as.Date("1936-02-29"),
                               ymin = -Inf, ymax = Inf, alpha = 0.15)
  }
  p <- p + ggplot2::geom_line(na.rm = TRUE) + ggplot2::geom_point(na.rm = TRUE, size = 1) +
    ggplot2::labs(x = NULL, y = "Occurrences per million words", colour = NULL, title = title)

  cols <- parl_party_colors()
  if (all(setdiff(unique(trend$grup), "(none)") %in% names(cols))) {
    p <- p + ggplot2::scale_colour_manual(values = c(cols, "(none)" = "grey60"))
  }
  if (identical(unique(trend$grup), "all")) p <- p + ggplot2::theme(legend.position = "none")
  p
}

# --- internals ----------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

# Accent folding, one character for one character, so that match positions
# in the folded text are valid in the original text.
parl_fold <- function(x, lower = TRUE) {
  x <- chartr(
    "\u00e0\u00e1\u00e2\u00e4\u00e8\u00e9\u00ea\u00eb\u00ec\u00ed\u00ee\u00ef\u00f2\u00f3\u00f4\u00f6\u00f9\u00fa\u00fb\u00fc\u00e7\u00f1\u00c0\u00c1\u00c2\u00c4\u00c8\u00c9\u00ca\u00cb\u00cc\u00cd\u00ce\u00cf\u00d2\u00d3\u00d4\u00d6\u00d9\u00da\u00db\u00dc\u00c7\u00d1\u00b7\u2019",
    "aaaaeeeeiiiioooouuuucnAAAAEEEEIIIIOOOOUUUUCN.'",
    x
  )
  if (lower) tolower(x) else x
}

parl_regex <- function(pattern, regex, ignore_accents) {
  if (!is.character(pattern) || length(pattern) != 1L || is.na(pattern) || !nzchar(pattern)) {
    cli::cli_abort("{.arg pattern} must be a single non-empty string.")
  }
  if (ignore_accents) pattern <- parl_fold(pattern, lower = FALSE)
  if (regex) pattern else paste0("\\Q", pattern, "\\E")
}

parl_count <- function(d, pattern, regex, ignore_case, ignore_accents) {
  rx <- parl_regex(pattern, regex, ignore_accents)
  hay <- if (ignore_accents) parl_folded_text(d) else d$text
  stringi::stri_count_regex(hay, rx, case_insensitive = ignore_case)
}

# Accent-folded text, cached for the whole text table.
parl_folded_text <- function(d) {
  txt <- parl_text_table()
  if (is.null(.parl_env$norm)) .parl_env$norm <- parl_fold(txt$text, lower = FALSE)
  out <- .parl_env$norm[match(parl_key(d), parl_key(txt))]
  out[is.na(out)] <- ""
  out
}

parl_last_words <- function(x, n) {
  w <- stringi::stri_split_regex(stringi::stri_trim_right(x), "\\s+")[[1]]
  paste(utils::tail(w[nzchar(w)], n), collapse = " ")
}

parl_first_words <- function(x, n) {
  w <- stringi::stri_split_regex(stringi::stri_trim_left(x), "\\s+")[[1]]
  paste(utils::head(w[nzchar(w)], n), collapse = " ")
}
