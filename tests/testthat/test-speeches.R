test_that("key figures match the codebook", {
  all <- parl_speeches(spoken_only = FALSE, text = FALSE)
  expect_equal(nrow(all), 32666)
  expect_equal(sum(all$assignacio_dubtosa), 304)
  expect_equal(sum(all$resolucio == "secretari_lectura"), 201)
  expect_equal(sum(all$paraules), 4568105)
  expect_equal(length(unique(all$sessio)), 259)
  expect_equal(nrow(parl_persons()), 89)
  expect_equal(nrow(parl_mps()), 83)
  expect_equal(nrow(parl_sessions()), 259)
})

test_that("filters work", {
  lr <- parl_speeches(year = 1934, party = "LR", text = FALSE)
  expect_true(nrow(lr) > 0)
  expect_true(all(lr$any == 1934 & lr$party_data == "LR"))

  per <- parl_speeches(from = "1936-03-01", to = "1936-07-31", text = FALSE)
  expect_true(all(per$data >= as.Date("1936-03-01") & per$data <= as.Date("1936-07-31")))

  spoken <- parl_speeches(text = FALSE)
  expect_false(any(spoken$resolucio %in% c("secretari_lectura", "anonim")))

  nochair <- parl_speeches(include_chair = FALSE, include_doubtful = FALSE, text = FALSE)
  expect_false(any(nochair$resolucio == "presidencia_sessio"))
  expect_false(any(nochair$assignacio_dubtosa))

  off <- parl_speeches(office = TRUE, text = FALSE)
  expect_true(all(!is.na(off$carrec)))
})

test_that("person matching accepts full names, ids and fragments", {
  a <- parl_speeches(person = "Lluís Companys i Jover", text = FALSE)
  b <- parl_speeches(person = "lluis_companys_i_jover", text = FALSE)
  expect_equal(nrow(a), nrow(b))
  expect_message(c <- parl_speeches(person = "rovira i roure", text = FALSE), "matched")
  expect_true(all(c$nom_sencer == "Joan Rovira i Roure"))
  expect_error(parl_speeches(person = "Nobody at all", text = FALSE), "No person")
})

test_that("invalid arguments give clear errors", {
  expect_error(parl_speeches(year = 1935, text = FALSE), "No sessions")
  expect_error(parl_speeches(party = "XYZ", text = FALSE), "Unknown")
  expect_error(parl_speeches(from = "not a date", text = FALSE), "date")
  expect_error(parl_speeches(spoken_only = NA, text = FALSE), "TRUE or FALSE")
})

test_that("government side is derived from governs.csv", {
  g <- parl_government_on("1934-01-05")
  expect_equal(g$parties[[1]], c("ERC", "PNRE", "ACR", "USC"))
  expect_equal(nrow(parl_government_on("1932-12-01")), 0)

  gov <- parl_speeches(side = "government", text = FALSE)
  opp <- parl_speeches(side = "opposition", text = FALSE)
  expect_true(all(gov$es_govern) && !any(opp$es_govern))
  expect_true(all(opp$party_data[opp$any == 1933] != "ERC"))

  x <- parl_add_government(data.frame(data = as.Date(c("1933-05-02", "1935-01-01")),
                                      party_data = c("LR", "ERC")))
  expect_identical(x$es_govern, c(FALSE, NA))
})

test_that("reference tables follow the new DADES schema", {
  m <- parl_ministers()
  expect_equal(nrow(m), 124L)
  expect_true(all(m$govern %in% parl_governments()$govern))
  expect_false("consellers_per_partit" %in% names(parl_governments()))
  expect_true("variants_capcalera" %in% names(parl_offices()))
  expect_false(any(grepl(" ", parl_parties()$anys_amb_diputats)))
  expect_false(any(grepl(";", parl_parties_by_year()$governs)))
})

test_that("office accepts harmonised labels and heading variants", {
  a <- parl_speeches(office = "Conseller de Treball", text = FALSE)
  b <- parl_speeches(office = c("CONSELLER DE TREBALL", "CONSELLER DEL TREBALL",
                                "CONSELLER DE TREBALL SENYORS DIPUTATS"), text = FALSE)
  expect_gt(nrow(a), 0)
  expect_equal(nrow(a), nrow(b))
})
