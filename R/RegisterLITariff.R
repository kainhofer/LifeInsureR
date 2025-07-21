#######################################################################################m#
# Data structure for tariffs (outsourced to its own, hidden environment/namespace)   ####
#######################################################################################m#

.pkgenv <- new.env(parent=emptyenv())
.pkgenv$Companies = list()
.pkgenv$Tariffs = list()
.pkgenv$Templates = list(
    include = "{{company}}_General.R"
)


#' Register a company with a given shortcut
#'
#' While the package allows individual tariff object to be created and used,
#' one can also register the tariffs for each company with the package.
#'
#' This function sets up the company data (including the short name) and other
#' parameters/features used for templating. Once a company is registered with a
#' short name (and optionally its long name and other parameters), one can register
#' tariffs/producs with the [register.tariff()] function.
#'
#' @param Company The (short) identifier for the company. Must contain only letters, digits and periods (i.e. a valid R identifier, given as string).
#' @param ... Additional parameters to be used for creating tariff templates, etc.
#'            Potential arguments are `name` for the full company name, `include`
#'            to define the company-specific include file for new product definitions.
#'
#' @return None
#'
#' @examples
#' register.company("ExVAG", name = "Example Versicherung AG")
#'
#' @export
register.company = function(Company, ...) {
    if (is.null(Company)) {
        warning("Company missing in call to register.company!");
        return();
    }
    if (is.null(.LIR.Companies[[Company]])) {
        .pkgenv$Companies[[Company]] = list(...)
    } else {
        .pkgenv$Companies[[Company]] = modifyList(.pkgenv$Companies[[Company]], list(...))
    }
    if (is.null(.pkgenv$Tariffs[[Company]])) {
        .pkgenv$Tariffs[[Company]] = list()
    }
}


#' Register a tariff with the package
#'
#' While the package allows individual tariff object to be created and used,
#' one can also register the tariffs for each company with the package.
#'
#' This function registers a [LifeInsuranceTarif] object with the `LiveInsureR`
#' package using a given ID, so automated scripts can then retrieve the
#' products/tariffs based on those IDs (e.g. tariff IDs used in the policy
#' administration systems) using the function [get.tariff()].
#'
#' @param Tarif The [LifeInsuranceTarif] object defining the tariff/product.
#' @param Company The (short) identifier for the company. Must contain only letters, digits and periods (i.e. a valid R identifier, given as string).
#' @param Produkt The short product/tariff ID to identify the tariff.
#' @param GV The (optional) profit group ("Gewinnverband") for the product.
#' @param AV The (optional) profit sub-group ("Abrechnungsveband") for the product
#'
#' @return The Tarif is returned, so this function can be chained to register the same object with multiple names or GV/AB combinations.
#'
#' @examples
#' library(MortalityTables)
#' library(magrittr)
#' mortalityTables.load("Austria_Annuities_AVOe2005R")
#'  Tariff.ImmAnnuity = InsuranceTarif$new(
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
#' @export
register.tariff = function(Tarif, Company, Produkt, GV = NULL, AV = NULL) {
    if (is.null(Produkt) || is.null(Company)) {
        warning("Product name or company missing in call to register.tariff!");
        return(Tarif);
    }
    if (is.null(.pkgenv$Tariffs[[Company]])) {
        .pkgenv$Tariffs[[Company]] = list()
    }
    .pkgenv$Tariffs[[Company]][[Produkt]] <- Tarif
    if (!is.null(GV) && !is.null(AV)) {
        .pkgenv$Tariffs[[Company]][[paste0(Produkt, "/GV", GV, "/AV", AV)]] <- Tarif
    }
    if (!is.null(GV)) {
        .pkgenv$Tariffs[[Company]][[paste0(Produkt, "/GV", GV)]] <- Tarif
    }
    if (!is.null(AV)) {
        .pkgenv$Tariffs[[Company]][[paste0(Produkt, "/AV", AV)]] <- Tarif
    }
    Tarif
}

#' Retrieve a LifeInsureR tariff registered with the package
#'
#' While the package allows individual tariff object to be created and used,
#' one can also register the tariffs for each company with the package.
#'
#' After registering the tariff/product with the function [register.tariff()],
#' this function can be used to retrieve the [LifeInsuranceTarif] object using
#' the given ID.
#'
#' @param Company The (short) identifier for the company. Must contain only letters, digits and periods (i.e. a valid R identifier, given as string).
#' @param Produkt The short product/tariff ID to identify the tariff.
#' @param GV The (optional) profit group ("Gewinnverband") for the product.
#' @param AV The (optional) profit sub-group ("Abrechnungsveband") for the product
#'
#' @return  The [LifeInsuranceTarif] object defining the tariff/product.
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
get.tariff = function(Company, Produkt, GV = NULL, AV = NULL) {
    if (is.null(Produkt) || is.null(Company)) {
        warning("Product name or company missing in call to get.tariff!");
        return(NULL);
    }
    if (is.null(.pkgenv$Tariffs[[Company]])) {
        return(NULL);
    }
    id = Produkt;
    if (!is.null(GV)) {
        id = paste0(id, "/GV", GV);
    }
    if (!is.null(AV)) {
        id = paste0(id, "/AV", AV);
    }
    .pkgenv$Tariffs[[Company]][[id]]
}


#' Retrieve a LifeInsureR tariff registered with the package
#'
#' While the package allows individual tariff object to be created and used,
#' one can also register the tariffs for each company with the package.
#'
#' After registering the tariff/product with the function [register.tariff()],
#' this function can be used to retrieve the [LifeInsuranceTarif] object using
#' the given ID.
#'
#' @param Company The (short) identifier for the company. Must contain only
#'                letters, digits and periods (i.e. a valid R identifier, given
#'                as string). If NULL, the list of all tariffs for all companies
#'                is returned.
#'
#' @return  The list of [LifeInsuranceTarif] objects registered for the company
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
#' get.tariffs("ExVAG")

#' @export
get.tariffs = function(Company = NULL) {
    if (!is.null(Company)) {
        .pkgenv$Tariffs[[Company]]
    } else {
        .pkgenv$Tariffs
    }
}

