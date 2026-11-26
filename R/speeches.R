#' Load and filter speeches
#'
#' Returns the speeches of the Parliament of Catalonia (1932-1938), one row per
#' speech, with the variables described in the codebook plus four derived
#' columns: `paraules` (number of words), `mes` (`"YYYY-MM"`), `govern`
#' (government in office on that date, from [parl_governments()]),
#' `es_govern` (whether `party_data` was in that government; `NA` when there
#' is no party, before the first statutory government of 1932-12-19, or while
#' the government was suspended) and `circumscripcio` (electoral district,
#' for MPs).
#'
#' Metadata are bundled with the package. The `text` column requires the text
#' to be downloaded once with [parl_download()].
#'
#' @param year Integer vector of years (1932-1938).
#' @param from,to Dates (or `"YYYY-MM-DD"` strings) delimiting the period.
#' @param person Character vector of persons: `nom_sencer` or `id_persona`.
#'   Values that match no person exactly are matched as a case- and
#'   accent-insensitive substring of `nom_sencer`.
#' @param party Character vector of party acronyms, matched against
#'   `party_data` (party on the date of the speech).
#' @param session Character vector of session identifiers (`sessio`).
#' @param side `"all"` (default), `"government"` or `"opposition"`, based on
#'   `es_govern`.
#' @param office `NULL` (default, no filter), `TRUE` (only speeches delivered
#'   in an office), `FALSE` (only speeches delivered as an MP) or a character
#'   vector of offices: either headings as they appear in the `carrec` column
#'   (e.g. `"CONSELLER DE CULTURA"`) or harmonised labels from the `carrec`
#'   column of [parl_offices()] (e.g. `"Conseller de Cultura"`), which match
#'   all their heading variants.
#' @param spoken_only Logical. If `TRUE` (default), drop readings by the
#'   secretary and anonymous headings, which are not speeches by a person.
#' @param include_chair Logical. If `FALSE`, drop speeches delivered while
#'   chairing the session (`resolucio == "presidencia_sessio"`).
#' @param include_doubtful Logical. If `FALSE`, drop speeches flagged in
#'   `assignacio_dubtosa`.
#' @param text Logical. Include the `text` column (default `TRUE`).
#'
#' @return A tibble.
#' @export
#' @examples
#' parl_speeches(year = 1934, party = "LR", text = FALSE)
#' parl_speeches(person = "Companys", include_chair = FALSE, text = FALSE)
parl_speeches <- function(year = NULL, from = NULL, to = NULL, person = NULL,
                          party = NULL, session = NULL,
                          side = c("all", "government", "opposition"),
                          office = NULL, spoken_only = TRUE, include_chair = TRUE,
                          include_doubtful = TRUE, text = TRUE) {
  side <- match.arg(side)
  parl_check_flag(spoken_only, "spoken_only")
  parl_check_flag(include_chair, "include_chair")
  parl_check_flag(include_doubtful, "include_doubtful")
  parl_check_flag(text, "text")

  d <- parl_meta
  keep <- rep(TRUE, nrow(d))

  if (!is.null(year)) {
    year <- parl_check_year(year)
    keep <- keep & d$any %in% year
  }
  if (!is.null(from)) keep <- keep & d$data >= parl_as_date(from, "from")
  if (!is.null(to))   keep <- keep & d$data <= parl_as_date(to, "to")
  if (!is.null(person)) keep <- keep & d$nom_sencer %in% parl_resolve_person(person)
  if (!is.null(party)) {
    parl_check_values(party, unique(stats::na.omit(d$party_data)), "party")
    keep <- keep & d$party_data %in% party
  }
  if (!is.null(session)) {
    parl_check_values(session, unique(d$sessio), "session")
    keep <- keep & d$sessio %in% session
  }
  if (side == "government") keep <- keep & d$es_govern %in% TRUE
  if (side == "opposition") keep <- keep & d$es_govern %in% FALSE
  if (!is.null(office)) {
    if (isTRUE(office)) {
      keep <- keep & !is.na(d$carrec)
    } else if (isFALSE(office)) {
      keep <- keep & is.na(d$carrec)
    } else {
      keep <- keep & d$carrec %in% parl_office_headings(office)
    }
  }
  if (spoken_only) keep <- keep & !d$resolucio %in% c("secretari_lectura", "anonim")
  if (!include_chair) keep <- keep & d$resolucio != "presidencia_sessio"
  if (!include_doubtful) keep <- keep & !d$assignacio_dubtosa

  d <- d[keep, ]
  if (text) d$text <- parl_text_for(d)
  tibble::as_tibble(d)
}

#' Sessions
#'
#' One row per session, with its date, number of speeches and words, and the
#' person who opened the chair.
#'
#' @return A tibble.
#' @export
#' @examples
#' parl_sessions()
parl_sessions <- function() {
  d <- parl_meta
  s <- split(seq_len(nrow(d)), d$sessio)
  first_chair <- parl_ref$chairs[parl_ref$chairs$ordre_des_de == 0, ]
  out <- data.frame(
    sessio = names(s),
    data = as.Date(vapply(s, function(i) as.character(d$data[i[1]]), character(1))),
    n_intervencions = lengths(s),
    paraules = vapply(s, function(i) sum(d$paraules[i]), numeric(1)),
    row.names = NULL
  )
  out$any <- as.integer(format(out$data, "%Y"))
  out$presidencia <- first_chair$nom_sencer[match(out$sessio, first_chair$sessio)]
  tibble::as_tibble(out[order(out$data, out$sessio), c("sessio", "data", "any", "n_intervencions", "paraules", "presidencia")])
}

#' Speakers
#'
#' One row per person who speaks at least once: party of the 1932 ticket,
#' whether an MP, electoral district, number of speeches and words (with and
#' without chairing the session), and first and last speech.
#'
#' @return A tibble.
#' @export
#' @examples
#' parl_persons()
parl_persons <- function() {
  d <- parl_meta[!is.na(parl_meta$nom_sencer), ]
  p <- parl_ref$persons
  s <- split(seq_len(nrow(d)), d$nom_sencer)[p$nom_sencer]
  chair <- d$resolucio == "presidencia_sessio"
  p$id_persona <- d$id_persona[match(p$nom_sencer, d$nom_sencer)]
  p$circumscripcio <- parl_ref$mps$circumscripcio[match(p$nom_sencer, parl_ref$mps$nom_sencer)]
  p$paraules <- vapply(s, function(i) sum(d$paraules[i]), numeric(1))
  p$n_sense_presidencia <- vapply(s, function(i) sum(!chair[i]), integer(1))
  p$paraules_sense_presidencia <- vapply(s, function(i) sum(d$paraules[i][!chair[i]]), numeric(1))
  p$primera <- as.Date(vapply(s, function(i) as.character(min(d$data[i])), character(1)))
  p$darrera <- as.Date(vapply(s, function(i) as.character(max(d$data[i])), character(1)))
  tibble::as_tibble(p)
}

parl_resolve_person <- function(person) {
  if (!is.character(person) || anyNA(person)) {
    cli::cli_abort("{.arg person} must be a character vector without missing values.")
  }
  names_all <- unique(stats::na.omit(parl_meta$nom_sencer))
  ids <- parl_meta$id_persona[match(names_all, parl_meta$nom_sencer)]
  out <- character(0)
  for (p in person) {
    if (p %in% names_all) {
      out <- c(out, p)
    } else if (p %in% ids) {
      out <- c(out, names_all[ids == p])
    } else {
      hit <- names_all[stringi::stri_detect_fixed(parl_fold(names_all), parl_fold(p))]
      if (!length(hit)) {
        cli::cli_abort(c("No person matches {.val {p}}.", "i" = "See {.code parl_persons()} for the list of speakers."))
      }
      cli::cli_inform("{.val {p}} matched: {.val {hit}}")
      out <- c(out, hit)
    }
  }
  unique(out)
}

# Expands harmonised office labels (parl_offices()$carrec) into the heading
# variants used in the speeches; other values are kept as given.
parl_office_headings <- function(office) {
  off <- parl_ref$offices
  hit <- off$carrec %in% office
  unique(c(office, unlist(strsplit(off$variants_capcalera[hit], "|", fixed = TRUE))))
}
