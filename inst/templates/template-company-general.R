library(tidyverse)

##########################################################################m#
# Datenstruktur für Tarife                                              ####
##########################################################################m#

#' @export
{{company}}.Tariffs = c()

#' @export
{{company}}.register = function(Tarif, Produkt, GV = NULL, AV = AVB, AVB = NULL) {
    if (is.null(Produkt)) {
        warning("Product name missing in call to {{company}}.register!");
        return(Tarif);
    }
    locked = FALSE
    # TODO: Unlocking does not work => Once the package is built/installed,
    #       new products/tariffs cannot be added via {{company}}.register.
    #       => Figure out a way to store all registered tariffs in a list
    #       that is not locked / that can be unlocked!
    if ("package:LifeInsureR{{company}}" %in% search()) {
        pkgenv = as.environment("package:LifeInsureR{{company}}")
        locked = bindingIsLocked("{{company}}.Tariffs", pkgenv)
        # if (locked) {
            # unlockBinding("{{company}}.Tariffs", pkgenv)
            # locked = bindingIsLocked("{{company}}.Tariffs", pkgenv)
        # }
        # nsenv = as.environment("namespace:LifeInsureR{{company}}")
        # locked = bindingIsLocked("{{company}}.Tariffs", pkgenv)
        # if (locked) {
        #     unlockBinding("{{company}}.Tariffs", pkgenv)
        #     locked = bindingIsLocked("{{company}}.Tariffs", pkgenv)
        # }
    }
    # assign("{{company}}.Tariffs", 123, -1, as.environment("package:LifeInsureR{{company}}"))
    # if (locked) {
        # warning("")
    # }
    # unlockBinding("{{company}}.Tariffs", as.environment("package:LifeInsureR{{company}}"))
    # unlockBinding({{company}}.Tariffs, pryr::where("{{company}}.Tariffs"))
    if (!locked && !is.null(GV) && !is.null(AV)) {
        {{company}}.Tariffs[[paste0(Produkt, "/GV", GV, "/AV", AV)]] <<- Tarif
    }
    if (!locked && !is.null(GV)) {
        {{company}}.Tariffs[[paste0(Produkt, "/GV", GV)]] <<- Tarif
    }
    if (!locked && !is.null(AVB)) {
        {{company}}.Tariffs[[paste0(Produkt, "/AV", AV)]] <<- Tarif
    }
    Tarif
}
#' @export
{{company}}.Tariff = function(Produkt, GV = NULL, AV = AVB, AVB = NULL) {
    if (is.null(Produkt)) {
        warning("Tariff name missing in call to {{company}}.Tariff!");
        return(NULL);
    }
    # if (is.null(GV) && is.null(AV)) {
    #     warning("Both profit class and terms indicator missing in call to {{company}}.Tariff!");
    #     return(NULL);
    # }
    id = Produkt;
    if (!is.null(GV)) {
        id = paste0(id, "/GV", GV);
    }
    if (!is.null(AV)) {
        id = paste0(id, "/AV", AV);
    }
    {{company}}.Tariffs[[id]]
}


##########################################################################m#
# GEWINNBETEILIGUNGSSÄTZE                                               ####
##########################################################################m#


{{company}}.Gesamtverzinsung = c(
    `2015` = 0.03, # TODO
    `2020` = 0.02,
    `2021` = 0.015,
    `2022` = 0.02
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
{{company}}.Sterbetafel2001 = function(params, ...) {
    if (params$ContractData$sex == "female") {
        mort.AT.census.2001.female
    } else {
        mort.AT.census.2001.male
    }
}

#' @export
{{company}}.Sterbetafel2001.unisex = mortalityTable.mixed(
    name = "ÖVSt 2000/02 unisex 70:30",
    table1 = mort.AT.census.2001.male, weight1 = 70,
    table2 = mort.AT.census.2001.female, weight2 = 30
)





##########################################################################m#
# Rententafel AVÖ 1996-R                                                ####

#' @export
{{company}}.AVOe1996R.AV = function(params, ...) {
    if (params$ContractData$sex == "female") {
        AVOe1996R.female.av325
    } else {
        AVOe1996R.male.av325
    }
}

#' @export
{{company}}.AVOe2005R = function(params, ...) {
    if (params$ContractData$sex == "female") {
        AVOe2005R.female
    } else {
        AVOe2005R.male
    }
}


#' @export
{{company}}.AVOe2005R.AV = function(params, ...) {
    if (params$ContractData$sex == "female") {
        AVOe2005R.female.av
    } else {
        AVOe2005R.male.av
    }
}






##########################################################################
# RÜCKKAUFSFORMELN                                                      ####
##########################################################################m#

#' @export
{{company}}.surrender.increasing90 = function(surrenderReserve, params, values) {
    n = params$ContractData$policyPeriod;
    surrenderReserve * (0.9 + 0.08*pmax(((0:n) - 3) / (n - 3), 0))
}
#' @export
{{company}}.surrender.98 = function(surrenderReserve, ...) {
    0.98 * surrenderReserve
}

##########################################################################m#
# KOSTENFORMELN                                                         ####
##########################################################################m#

#' @export
{{company}}.costs = initializeCosts(
    alpha = 0.04, Zillmer = 0.024,
    beta = 0.03,
    gamma = 0.001,
    gamma.paidUp = 0.0015,
    gamma.premiumfree = 0.002
)

#' @export
{{company}}.UJZ = list("1" = 0, "2" = 0.01, "4" = 0.02, "12" = 0.03)

##########################################################################m#
# RABATTFORMELN                                                         ####
##########################################################################m#

#' @export
{{company}}.premiumRebate =  function(params, values) {
    prem = values$premiums[["gross"]]
    # TODO
    if (prem >= 1000) {
        0.1
    } else if (prem >= 500) {
        0.05
    } else {
        0
    }
}


##########################################################################m#
# TECHNISCHES ALTER                                                     ####
##########################################################################m#

#' z.B. Frauen werden um 5 Jahre verjüngt, Mindestalter 20
#' etc.




##########################################################################m#
# TARIFSPEZIFIKA (HOOKS) für {{company}}                                    ####
##########################################################################m#

