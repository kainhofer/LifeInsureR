testthat::test_that("LIC_debug_options index is up to date", {
  # Find package root from the testthat directory
  # pkg_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))

  pkg_root = here::here()
  extracted <- extract_lic_debug_options(pkg_root)

  stored <- get0("LIC_debug_options", envir = asNamespace("LifeInsureR"), inherits = FALSE)
  testthat::expect_true(!is.null(stored) && length(stored) > 0)

  testthat::expect_identical(extracted, stored)
})
