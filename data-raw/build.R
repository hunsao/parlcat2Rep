# Builds the internal data of parlcat2Rep from the published release.
#
# Input:  the release folder VOICES_REPUBLIC_1932_1938_v1.0 (set RELEASE below).
# Output: R/sysdata.rda                 speech metadata (no text) + reference tables
#         inst/extdata/parl_text_sample.rds   small text sample for examples/tests
#         data-raw/out/parl_text_<v>.rds      full text asset, to publish as a release asset
#
# Run from the package root: source("data-raw/build.R")

RELEASE <- "C:/Users/David/Desktop/UPF/ALTRES_RODON/ENTREGAS/VOICES_REPUBLIC_1932_1938_v1.0"
DATASET_VERSION <- "1.0"
SAMPLE_SESSION <- "DSPC_1933-03-07_Sessio_023"

read_release <- function(...) {
  df <- utils::read.csv(file.path(RELEASE, ...), fileEncoding = "UTF-8-BOM",
                        stringsAsFactors = FALSE, na.strings = "")
  # Python-style booleans ("True"/"False") to logical
  for (nm in names(df)) {
    v <- df[[nm]]
    if (is.character(v) && all(v %in% c("True", "False", NA)) && any(!is.na(v))) df[[nm]] <- v == "True"
  }
  tibble::as_tibble(df)
}

# --- speeches -----------------------------------------------------------------
sp <- read_release("INTERVENCIONS", "intervencions_1932_1938.csv")
stopifnot(nrow(sp) == 32666L, !anyDuplicated(sp[c("sessio", "ordre")]))
sp$data <- as.Date(sp$data)
sp$text[is.na(sp$text)] <- ""
sp$paraules <- stringi::stri_count_regex(sp$text, "\\S+")
sp$mes <- format(sp$data, "%Y-%m")

# --- reference tables ---------------------------------------------------------
ref <- list(
  mps             = read_release("DADES", "diputats.csv"),
  parties         = read_release("DADES", "partits.csv"),
  parties_by_year = read_release("DADES", "partits_per_any.csv"),
  governments     = read_release("DADES", "governs.csv"),
  offices         = read_release("DADES", "carrecs.csv"),
  party_changes   = read_release("DADES", "canvis_partit.csv"),
  chairs          = read_release("DADES", "presidencia_sessions.csv"),
  aliases         = read_release("DADES", "alias_oradors.csv"),
  review_cases    = read_release("FONTS_I_REVISIO", "review_cases.csv"),
  persons         = read_release("INTERVENCIONS", "_persones.csv")
)
for (nm in c("governments", "offices")) {
  ref[[nm]]$data_inici <- as.Date(ref[[nm]]$data_inici)
  ref[[nm]]$data_fi    <- as.Date(ref[[nm]]$data_fi)
}
ref$party_changes$data_inici <- as.Date(ref$party_changes$data_inici)

# --- derived columns ----------------------------------------------------------
gov <- ref$governments
idx <- vapply(sp$data, function(d) {
  i <- which(gov$data_inici <= d & gov$data_fi >= d)
  if (length(i)) i[1] else NA_integer_
}, integer(1))
sp$govern <- gov$govern[idx]
gov_parties <- strsplit(ifelse(is.na(gov$partits), "", gov$partits), "|", fixed = TRUE)
# NA when there is no party, no statutory government yet (before 1932-12-19)
# or the government is suspended (1934-10-14 to 1936-02-29).
no_gov <- is.na(idx) | is.na(gov$partits[idx])
sp$es_govern <- ifelse(
  is.na(sp$party_data) | no_gov, NA,
  mapply(function(p, i) p %in% gov_parties[[i]], sp$party_data, ifelse(is.na(idx), 1L, idx))
)
sp$circumscripcio <- ref$mps$circumscripcio[match(sp$nom_sencer, ref$mps$nom_sencer)]

# --- split metadata and text --------------------------------------------------
parl_meta <- sp[setdiff(names(sp), "text")]
text <- sp[c("sessio", "ordre", "text")]

dir.create("data-raw/out", showWarnings = FALSE)
text_file <- file.path("data-raw/out", sprintf("parl_text_v%s.rds", DATASET_VERSION))
saveRDS(text, text_file, compress = "xz")

parl_text_info <- list(
  version = DATASET_VERSION,
  file    = basename(text_file),
  url     = sprintf("https://github.com/hunsao/parlcat2Rep/releases/download/data-v%s/%s",
                    DATASET_VERSION, basename(text_file)),
  sha256  = unname(cli::hash_file_sha256(text_file)),
  n       = nrow(text)
)

dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
saveRDS(text[text$sessio == SAMPLE_SESSION, ], "inst/extdata/parl_text_sample.rds", compress = "xz")

parl_ref <- ref
usethis::use_data(parl_meta, parl_ref, parl_text_info, internal = TRUE,
                  overwrite = TRUE, compress = "xz")

message("Text asset: ", text_file, " (", round(file.size(text_file) / 1e6, 2), " MB)")
message("Sample: ", round(file.size("inst/extdata/parl_text_sample.rds") / 1e3), " KB")
