# parlcat2Rep (development version)

* First working version, for dataset *Voices of the Republic* v1.0.
* Speech metadata and reference tables bundled with the package; the text of
  the speeches (about 6 MB) is downloaded once with `parl_download()`.
* Filtering by year, dates, person, party on the date, session, office and
  government/opposition (`parl_speeches()`).
* Text search, keyword in context and trends (`parl_search()`, `parl_kwic()`,
  `parl_trend()`, `parl_plot_trend()`), and conversion to 'quanteda' and
  'tidytext'.
