test_that("demo.adj() works", {

  kwb.plot:::demo.adj()
})

test_that("preview_themes() works", {

  kwb.plot:::preview_themes()
})

test_that("ggplot_themes() works", {

  kwb.plot:::ggplot_themes()
})

test_that("apply_elements_text() works", {

  expect_error(kwb.plot:::apply_elements_text())
})

test_that("demo_theme_properties() works", {

  kwb.plot:::demo_theme_properties()
})

test_that("demo_themes_text() works", {

  kwb.plot:::demo_themes_text()
})

test_that("demo_themes_rect() works", {

  kwb.plot:::demo_themes_rect()
})

test_that("demo_themes_line() works", {

  kwb.plot:::demo_themes_line()
})

test_that("element_types() works", {

  kwb.plot:::element_types()
})

test_that("to_element_themes() works", {

  expect_error(kwb.plot:::to_element_themes())
})
