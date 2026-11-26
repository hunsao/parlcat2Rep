local_sample <- function(env = parent.frame()) {
  old <- parl_use_sample(TRUE)
  withr::defer(parl_use_sample(old), envir = env)
}

test_that("text is required unless the sample or text = FALSE is used", {
  withr::local_envvar(PARLCAT2REP_CACHE = withr::local_tempdir())
  parl_use_sample(FALSE)
  expect_error(parl_speeches(year = 1938), "parl_download")
  expect_no_error(parl_speeches(year = 1938, text = FALSE))
})

test_that("search is accent and case insensitive", {
  local_sample()
  a <- parl_search("seu d'urgell", session = "DSPC_1933-03-07_Sessio_023")
  b <- parl_search("Seu d'Urgell", session = "DSPC_1933-03-07_Sessio_023")
  expect_true(nrow(a) > 0)
  expect_equal(nrow(a), nrow(b))
  expect_true(all(a$n_matches > 0))
  expect_equal(nrow(parl_search("Catalunya", ignore_accents = FALSE, session = "DSPC_1933-03-07_Sessio_023")),
               nrow(parl_search("catalunya", session = "DSPC_1933-03-07_Sessio_023")))
})

test_that("kwic returns one row per occurrence", {
  local_sample()
  s <- parl_search("Urgell")
  k <- parl_kwic("Urgell", window = 3)
  expect_equal(nrow(k), sum(s$n_matches))
  expect_true(all(grepl("urgell", tolower(k$match))))
  expect_true(all(lengths(strsplit(k$pre, " ")) <= 3))
})

test_that("trend rates add up", {
  local_sample()
  tr <- parl_trend("Govern", by = "year")
  expect_equal(sum(tr$n_matches), sum(parl_search("Govern")$n_matches))
  expect_true(all(is.na(tr$rate[tr$paraules == 0])))
  expect_s3_class(parl_plot_trend(tr), "ggplot")
})

test_that("conversions keep metadata", {
  local_sample()
  d <- parl_speeches(session = "DSPC_1933-03-07_Sessio_023")
  skip_if_not_installed("quanteda")
  corp <- parl_as_corpus(d)
  expect_equal(quanteda::ndoc(corp), nrow(d))
  expect_true("party_data" %in% names(quanteda::docvars(corp)))
  skip_if_not_installed("tidytext")
  tk <- parl_tidy(d)
  expect_true(all(c("word", "nom_sencer") %in% names(tk)))
})

test_that("download checks the checksum", {
  withr::local_envvar(PARLCAT2REP_CACHE = withr::local_tempdir())
  bad <- withr::local_tempfile(fileext = ".rds")
  saveRDS(data.frame(x = 1), bad)
  expect_error(parl_download(file = bad, quiet = TRUE), "checksum")
})
