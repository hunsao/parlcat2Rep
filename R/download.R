.parl_env <- new.env(parent = emptyenv())

#' Cache directory for the speech text
#'
#' The full text of the speeches is not shipped with the package. It is
#' downloaded once with [parl_download()] and stored here. Set the environment
#' variable `PARLCAT2REP_CACHE` to use another folder.
#'
#' @return The path of the cache directory (character).
#' @export
parl_cache_dir <- function() {
  Sys.getenv("PARLCAT2REP_CACHE", tools::R_user_dir("parlcat2Rep", which = "cache"))
}

#' Download the text of the speeches
#'
#' Downloads the text of all speeches (about 6 MB) for the dataset version
#' bundled with the package, checks its SHA-256 and stores it in
#' [parl_cache_dir()]. It only needs to be run once per dataset version.
#'
#' @param file Optional path to a local copy of the text asset. When given,
#'   the file is checked and copied to the cache instead of downloading it.
#' @param force Logical. Download again even if the text is already cached.
#' @param quiet Logical. Suppress progress messages.
#'
#' @return The path of the cached file, invisibly.
#' @export
#' @examples
#' \dontrun{
#' parl_download()
#' }
parl_download <- function(file = NULL, force = FALSE, quiet = FALSE) {
  info <- parl_text_info
  dest <- parl_text_path()

  if (file.exists(dest) && !isTRUE(force)) {
    if (!quiet) cli::cli_inform("Text already downloaded: {.file {dest}}")
    return(invisible(dest))
  }

  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)

  if (!is.null(file)) {
    if (!file.exists(file)) cli::cli_abort("File not found: {.file {file}}")
    file.copy(file, tmp, overwrite = TRUE)
  } else {
    if (!quiet) cli::cli_inform("Downloading the speech text (v{info$version}) from {.url {info$url}}")
    ok <- tryCatch(
      utils::download.file(info$url, tmp, mode = "wb", quiet = quiet) == 0L,
      error = function(e) FALSE, warning = function(w) FALSE
    )
    if (!ok) {
      cli::cli_abort(c(
        "Could not download the speech text.",
        "i" = "Check your internet connection, or pass a local copy with {.code parl_download(file = ...)}."
      ))
    }
  }

  hash <- unname(cli::hash_file_sha256(tmp))
  if (!identical(hash, info$sha256)) {
    cli::cli_abort(c(
      "The text file does not match the expected checksum.",
      "x" = "SHA-256 {.val {hash}}",
      "i" = "Expected {.val {info$sha256}} (dataset v{info$version})."
    ))
  }

  file.copy(tmp, dest, overwrite = TRUE)
  .parl_env$text <- NULL
  if (!quiet) cli::cli_inform(c("v" = "Text stored in {.file {dest}}"))
  invisible(dest)
}

#' Remove the cached text
#'
#' @return `TRUE` invisibly.
#' @export
parl_cache_clear <- function() {
  unlink(parl_text_path())
  .parl_env$text <- NULL
  .parl_env$norm <- NULL
  invisible(TRUE)
}

#' Use the bundled text sample
#'
#' Switches the package to a small sample of the text (one session,
#' `DSPC_1933-03-07_Sessio_023`) bundled with the package, so that examples
#' and tests run without downloading anything. Speeches outside the sample get
#' an empty `text`.
#'
#' @param use Logical. `TRUE` to use the sample, `FALSE` to go back to the
#'   downloaded text.
#' @return The previous value, invisibly.
#' @export
#' @examples
#' old <- parl_use_sample()
#' parl_search("Seu d'Urgell")
#' parl_use_sample(old)
parl_use_sample <- function(use = TRUE) {
  old <- getOption("parlcat2Rep.sample", FALSE)
  options(parlcat2Rep.sample = isTRUE(use))
  .parl_env$text <- NULL
  .parl_env$norm <- NULL
  invisible(old)
}

parl_text_path <- function() {
  file.path(parl_cache_dir(), parl_text_info$file)
}

parl_using_sample <- function() isTRUE(getOption("parlcat2Rep.sample", FALSE))

# Text table (sessio, ordre, text), loaded once per session.
parl_text_table <- function() {
  if (!is.null(.parl_env$text)) return(.parl_env$text)

  if (parl_using_sample()) {
    txt <- readRDS(system.file("extdata", "parl_text_sample.rds", package = "parlcat2Rep"))
  } else {
    path <- parl_text_path()
    if (!file.exists(path)) {
      cli::cli_abort(c(
        "The text of the speeches has not been downloaded yet.",
        "i" = "Run {.code parl_download()} once, or use {.code text = FALSE}.",
        "i" = "For a quick try without downloading, use {.code parl_use_sample()}."
      ))
    }
    txt <- readRDS(path)
  }
  .parl_env$text <- txt
  .parl_env$norm <- NULL
  txt
}

# Text aligned to `data` rows ("" when missing).
parl_text_for <- function(data) {
  txt <- parl_text_table()
  i <- match(parl_key(data), parl_key(txt))
  out <- txt$text[i]
  out[is.na(out)] <- ""
  out
}

parl_key <- function(x) paste(x$sessio, x$ordre, sep = "#")
