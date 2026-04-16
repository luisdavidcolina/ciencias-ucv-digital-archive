context("Filtrado de Datos")

test_that("split_doc_types separa correctamente por punto y coma", {
  expect_equal(split_doc_types("Tipo1; Tipo2"), c("Tipo1", "Tipo2"))
  expect_equal(split_doc_types("  TipoA ;TipoB  "), c("TipoA", "TipoB"))
  expect_equal(split_doc_types(""), character(0))
})

test_that("filter_archivo_data filtra correctamente por titulo", {
  # Mock de datos
  df <- data.frame(
    titulo = c("Documento Alfa", "Archivo Beta", "Gamma"),
    autor = c("Autor 1", "Autor 2", "Autor 3"),
    doc_type = c("T1", "T2", "T1"),
    fecha = c("2021-01-01", "2022-01-01", "2023-01-01"),
    stringsAsFactors = FALSE
  )
  
  res <- filter_archivo_data(df, "Alfa", NULL, NULL)
  expect_equal(nrow(res), 1)
  expect_equal(res$titulo, "Documento Alfa")
  
  res_vacio <- filter_archivo_data(df, "Zeta", NULL, NULL)
  expect_equal(nrow(res_vacio), 0)
})

test_that("row_has_any_doc_type funciona con selecciones multiples", {
  expect_true(row_has_any_doc_type("T1; T2", c("T1")))
  expect_true(row_has_any_doc_type("T1; T2", c("T2")))
  expect_false(row_has_any_doc_type("T1", c("T2")))
  expect_true(row_has_any_doc_type("T1", NULL))
})
