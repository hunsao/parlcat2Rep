#' Dataset and package metadata
#'
#' @return A named list: dataset version, coverage, key figures, whether the
#'   text has been downloaded, cache location and source URL.
#' @export
#' @examples
#' parl_metadata()
parl_metadata <- function() {
  d <- parl_meta
  list(
    package          = "parlcat2Rep",
    dataset          = "Voices of the Republic: A Speech Corpus of the Parliament of Catalonia (1932-1938)",
    dataset_version  = parl_text_info$version,
    n_speeches       = nrow(d),
    n_sessions       = length(unique(d$sessio)),
    n_words          = sum(d$paraules),
    n_persons        = length(unique(stats::na.omit(d$nom_sencer))),
    period           = range(d$data),
    years            = sort(unique(d$any)),
    text_downloaded  = file.exists(parl_text_path()),
    using_sample     = parl_using_sample(),
    cache_dir        = parl_cache_dir(),
    text_url         = parl_text_info$url,
    licence          = "Data: CC BY 4.0 (draft); code: MIT",
    note             = "Please cite both the dataset and this package (draft citation, not final)."
  )
}

#' Party colours
#'
#' A fixed colour for each party acronym, to use in plots. Provisional: to be
#' aligned with the project website.
#'
#' @return A named character vector of hex colours.
#' @export
#' @examples
#' parl_party_colors()
parl_party_colors <- function() {
  c(
    ERC  = "#E3A21A", LR   = "#1F3A5F", USC  = "#C0392B", PSUC = "#7B1E1E",
    PNRE = "#8C6D1F", ACR  = "#2E7D5B", PRA  = "#7D3C98", UR   = "#6B8E23",
    UDC  = "#4F6D8A", UC   = "#A0522D", PRDF = "#D35400", IND  = "#7F7F7F"
  )
}
