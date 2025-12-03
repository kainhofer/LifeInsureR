library(tidyverse)

##########################################################################m#
# Datenstruktur für Tarife                                              ####
##########################################################################m#

# Idea / approach taken from the `memoise` package (In-memory cache)
# XXXCOMPANYXXX.Tariffs.init = function() {
#     tariffs = new.env(TRUE, emptyenv())
#     tariff_set = function(key, value) {
#         assign(key, value, envir = tariffs)
#     }
#     tariff_get = function(key) {
#         get(key, envir = tariffs, inherits = FALSE)
#     }
#     tariff_exists = function(key) {
#         exists(key, envir = tariffs)
#     }
#     list(
#         get = tariff_get,
#         set = tariff_set,
#         exists =
#     )
# }
#' @export
XXXCOMPANYXXX.Tariffs = c()

#' @export
XXXCOMPANYXXX.register = function(Tarif, Produkt, GV = NULL, AVB = NULL) {
    if (is.null(Produkt)) {
        warning("Product name missing in call to XXXCOMPANYXXX.register!");
        return();
    }
    locked = FALSE
    # TODO: Unlocking does not work => Once the package is built/installed,
    #       new products/tariffs cannot be added via XXXCOMPANYXXX.register.
    #       => Figure out a way to store all registered tariffs in a list
    #       that is not locked / that can be unlocked!
    if ("package:LifeInsureRXXXCOMPANYXXX" %in% search()) {
        pkgenv = as.environment("package:LifeInsureRXXXCOMPANYXXX")
        locked = bindingIsLocked("XXXCOMPANYXXX.Tariffs", pkgenv)
        # if (locked) {
        # unlockBinding("XXXCOMPANYXXX.Tariffs", pkgenv)
        # locked = bindingIsLocked("XXXCOMPANYXXX.Tariffs", pkgenv)
        # }
        # nsenv = as.environment("namespace:LifeInsureRXXXCOMPANYXXX")
        # locked = bindingIsLocked("XXXCOMPANYXXX.Tariffs", pkgenv)
        # if (locked) {
        #     unlockBinding("XXXCOMPANYXXX.Tariffs", pkgenv)
        #     locked = bindingIsLocked("XXXCOMPANYXXX.Tariffs", pkgenv)
        # }
    }
    # assign("XXXCOMPANYXXX.Tariffs", 123, -1, as.environment("package:LifeInsureRXXXCOMPANYXXX"))
    # if (locked) {
        # warning("")
    # }
    # unlockBinding("XXXCOMPANYXXX.Tariffs", as.environment("package:LifeInsureRXXXCOMPANYXXX"))
    # unlockBinding(XXXCOMPANYXXX.Tariffs, pryr::where("XXXCOMPANYXXX.Tariffs"))
    if (!locked && !is.null(GV) && !is.null(AVB)) {
        XXXCOMPANYXXX.Tariffs[[paste0(Produkt, "/GV", GV, "/AVB", AVB)]] <<- Tarif
    }
    if (!locked && !is.null(GV)) {
        XXXCOMPANYXXX.Tariffs[[paste0(Produkt, "/GV", GV)]] <<- Tarif
    }
    if (!locked && !is.null(AVB)) {
        XXXCOMPANYXXX.Tariffs[[paste0(Produkt, "/AVB", AVB)]] <<- Tarif
    }
    Tarif
}
#' @export
XXXCOMPANYXXX.Tariff = function(Produkt, GV = NULL, AVB = NULL) {
    if (is.null(Produkt)) {
        warning("Tariff name missing in call to XXXCOMPANYXXX.Tariff!");
        return(NULL);
    }
    if (is.null(GV) && is.null(AVG)) {
        warning("Both profit class and terms indicator missing in call to XXXCOMPANYXXX.Tariff!");
        return(NULL);
    }
    id = Produkt;
    if (!is.null(GV)) {
        id = paste0(id, "/GV", GV);
    }
    if (!is.null(AVB)) {
        id = paste0(id, "/AVB", AVB);
    }
    XXXCOMPANYXXX.Tariffs[[id]]
}


##########################################################################m#
# GEWINNBETEILIGUNGSSÄTZE                                               ####
##########################################################################m#


XXXCOMPANYXXX.Gesamtverzinsung = c(
    `2000` = 0.07,
    `2005` = 0.05,
    `2006` = 0.045,
    `2005` = 0.04,
    `2006` = 0.035,
    `2007` = 0.03,
    `2012` = 0.025,
    `2015` = 0.02,
    `2020` = 0.0175,
    `2023` = 0.015
)


##########################################################################m#
# STERBETAFELN                                                          ####
##########################################################################m#


mortalityTables.load("Austria_Census")
mortalityTables.load("Austria_Annuities_EROMF")
mortalityTables.load("Austria_Annuities_AVOe2005R")
mortalityTables.load("Austria_Annuities_AVOe1996R")


##########################################################################m#
# Österr. Volkssterbetafel 2000/02                                      ####

#' @export
XXXCOMPANYXXX.Sterbetafel2001 = function(params, ...) {
    if (params$ContractData$sex == "female") {
        mort.AT.census.2001.female
    } else {
        mort.AT.census.2001.male
    }
}

#' @export
XXXCOMPANYXXX.Sterbetafel2001.unisex = mortalityTable.mixed(
    name = "ÖVSt 2000/02 unisex 70:30",
    table1 = mort.AT.census.2001.male, weight1 = 70,
    table2 = mort.AT.census.2001.female, weight2 = 30
)





##########################################################################m#
# Rententafel AVÖ 1996-R                                                ####

#' @export
XXXCOMPANYXXX.AVOe1996R.AV = function(params, ...) {
    if (params$ContractData$sex == "female") {
        AVOe1996R.male.av325
    } else {
        AVOe1996R.female.av325
    }
}






##########################################################################
# RÜCKKAUFSFORMELN                                                      ####
##########################################################################m#

#' @export
XXXCOMPANYXXX.surrender.increasing90 = function(surrenderReserve, params, values) {
  n = values$contract$getPolicyTerm()
  surrenderReserve * (0.9 + 0.08*pmax(((0:n) - 3) / (n - 3), 0))
}


##########################################################################m#
# KOSTENFORMELN                                                         ####
##########################################################################m#



##########################################################################m#
# RABATTFORMELN                                                         ####
##########################################################################m#



##########################################################################m#
# TECHNISCHES ALTER                                                     ####
##########################################################################m#

#' z.B. Frauen werden um 5 Jahre verjüngt, Mindestalter 20
#' etc.




##########################################################################m#
# TARIFSPEZIFIKA (HOOKS) der XXXCOMPANYXXX                                    ####
##########################################################################m#

