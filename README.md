# parlcat2Rep

> **Development version.** Citation, licence of the data and maintainer details are drafts.

R package for working with *Voices of the Republic*, the speech corpus of the
Parliament of Catalonia during the Spanish Second Republic (1932–1938):
32,666 speeches, 259 sessions, 4.57 million words, with speaker, party and
office on the date of each speech.

## Installation (development)

```r
# install.packages("pak")
pak::pak("hunsao/parlcat2Rep")
```

## Quick example

```r
library(parlcat2Rep)

# Metadata and reference tables work offline
parl_speeches(year = 1934, party = "LR", text = FALSE)
parl_governments()

# The text (about 6 MB) is downloaded once and cached
parl_download()

parl_search("rabassaires", include_chair = FALSE)
parl_kwic("Estatut", window = 8, year = 1932)

parl_trend("ordre públic", group = "side") |>
  parl_plot_trend()

# Text-analysis formats
corp <- parl_as_corpus(parl_speeches(year = 1936))   # quanteda
toks <- parl_tidy(parl_speeches(year = 1938))        # tidytext
```

Without downloading anything, `parl_use_sample()` switches to a bundled sample
of one session.

## Before counting

- Use `party_data` (party on the date of the speech), not `party` (1932 ticket).
- `include_chair = FALSE` leaves out the work of chairing the session.
- `spoken_only = TRUE` (default) leaves out readings by the secretary and anonymous headings.
- `assignacio_dubtosa` flags 304 speeches whose attribution has a known limitation.

See the codebook of the dataset for all variables.
