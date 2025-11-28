#' @include RegisterLITariff.R
#'
#' @import dplyr
NULL

#' Create template code files for a new tarif
#'
#' While the package allows individual tariff object to be created and used,
#' one can also register the tariffs for each company with the package.
#'
#' After registering the tariff/product with the function [register.tariff()],
#' this function can be used to retrieve the [InsuranceTarif] object using
#' the given ID.
#'
#' @param Company The (short) identifier for the company. Must contain only letters, digits and periods (i.e. a valid R identifier, given as string).
#' @param Produkt The short product/tariff ID to identify the tariff.
#' @param GV The (optional) profit group ("Gewinnverband") for the product.
#' @param AV The (optional) profit sub-group ("Abrechnungsveband") for the product
#'
#' @return  The [InsuranceTarif] object defining the tariff/product.
#'
#' @examples
#' library(MortalityTables)
#' library(magrittr)
#' mortalityTables.load("Austria_Annuities_AVOe2005R")
#' InsuranceTarif$new(
#'     type = "annuity",
#'     # tarif = "Ann1",
#'     # desc = "An annuity with single-premium",
#'     premiumPeriod = 1,
#'     mortalityTable = AVOe2005R.unisex,
#'     i = 0.005
#' ) %>%
#'      register.tariff(Company = "ExVAG", Produkt = "ImmAnn1") %>%
#'      register.tariff(Company = "ExVAG", Produkt = "ImmAnn1_BV")
#'
#'  Tariff.ImmAnnuity = get.tariff("ExVAG", "ImmAnn1")
#' @export
prepare.tariff = function(Company, Tariff, type = "endowment", destfile = NULL) {
    if (is.null(destfile)) {
        destfile = Tariff
    }
    replacements = list() %>%
        modifyList(.pkgenv$Templates) %>%
        modifyList(.pkgenv$Companies[[Company]]) %>%
        modifyList(list(company = Company, tarif = Tariff, type = type, destfile = destfile))

    do_replacements = function(text, replacements) {
        pattern = "\\{\\{([-.0-9a-zA-Z]+)\\}\\}"
        str_replace_all(
            text,
            pattern,
            replacement = function(val, ...) {
                key = str_replace_all(val, pattern, "\\1")
                if (!is.null(replacements[[key]])) {
                    replacements[[key]]
                } else {
                    val
                }
            })
    }
    do_replacements.file = function(from, to, replacements, ...) {
        # Read template file, replace and store to destination
        suppressWarnings(tx <- readLines(from))
        tx2 = do_replacements(tx, replacements, ...)
        cat(tx2, file = to, append = TRUE, sep = "\n")
    }

    # Replacements (e.g. company-specific include file) can contain other replacements
    replacements = replacements %>% do_replacements(replacements)


    template.tarifHeader = system.file("templates", "template-tarif-header.R", package = pkgload::pkg_name())
    template.tarif = system.file("templates", "template-tarif.R", package = pkgload::pkg_name())
    template.test = system.file("templates", "template-test-tarif.R", package = pkgload::pkg_name())
    template.companyGeneral = system.file("templates", "template-company-general.R", package = pkgload::pkg_name())

    # File names can contain parameters!
    outfile.companyGeneral = here("R", "{{company}}_General.R") %>%
        do_replacements(replacements)
    outfile.tarif = here("R", "{{company}}_{{destfile}}.R") %>%
        do_replacements(replacements)
    outfile.test = here("tests", "testthat", "test-{{company}}-{{destfile}}.R") %>%
        do_replacements(replacements)


    # Generate company_General.R if it does not exist:
    companyGeneral.created = FALSE
    if (!file.exists(outfile.companyGeneral)) {
        companyGeneral.created = TRUE
        do_replacements.file(template.companyGeneral, outfile.companyGeneral, replacements)
    }

    # Insert the tarif template into the outfile. If the outfile does not exist,
    # create the appropriate header (loading the company-specific headers etc.)
    if (!file.exists(outfile.tarif)) {
        do_replacements.file(template.tarifHeader, outfile.tarif, replacements)
    }
    do_replacements.file(template.tarif, outfile.tarif, replacements)

    # Create the test template
    do_replacements.file(template.test, outfile.test, replacements)

    if ((system.file(package='rstudioapi') == "") && rstudioapi::isAvailable()) {
        if (companyGeneral.created) {
            rstudioapi::documentOpen(outfile.companyGeneral)
        }
        rstudioapi::documentOpen(outfile.tarif)
        rstudioapi::documentOpen(outfile.test)
    }
}


