#' Reference tables
#'
#' The tables used to identify speakers and assign parties, as published in
#' the `DADES/` and `FONTS_I_REVISIO/` folders of the dataset. They are bundled
#' with the package and need no download. Their columns are described in the
#' codebook.
#'
#' - `parl_mps()`: the 83 MPs elected in 1932 (`diputats.csv`).
#' - `parl_parties()`: party acronyms (`partits.csv`).
#' - `parl_parties_by_year()`: MPs and government status per year and party
#'   (`partits_per_any.csv`).
#' - `parl_governments()`: governments of the Generalitat and their parties
#'   (`governs.csv`).
#' - `parl_offices()`: holders of the offices that speak in the *Diari*
#'   (`carrecs.csv`).
#' - `parl_party_changes()`: dated party changes (`canvis_partit.csv`).
#' - `parl_chairs()`: who chairs each session and from which speech
#'   (`presidencia_sessions.csv`).
#' - `parl_aliases()`: speaker headings resolved by hand (`alias_oradors.csv`).
#' - `parl_review_cases()`: doubtful cases and the decisions taken
#'   (`review_cases.csv`).
#'
#' @return A tibble.
#' @name parl_reference
#' @examples
#' parl_governments()
#' parl_mps()
NULL

#' @rdname parl_reference
#' @export
parl_mps <- function() parl_ref$mps

#' @rdname parl_reference
#' @export
parl_parties <- function() parl_ref$parties

#' @rdname parl_reference
#' @export
parl_parties_by_year <- function() parl_ref$parties_by_year

#' @rdname parl_reference
#' @export
parl_governments <- function() parl_ref$governments

#' @rdname parl_reference
#' @export
parl_offices <- function() parl_ref$offices

#' @rdname parl_reference
#' @export
parl_party_changes <- function() parl_ref$party_changes

#' @rdname parl_reference
#' @export
parl_chairs <- function() parl_ref$chairs

#' @rdname parl_reference
#' @export
parl_aliases <- function() parl_ref$aliases

#' @rdname parl_reference
#' @export
parl_review_cases <- function() parl_ref$review_cases
