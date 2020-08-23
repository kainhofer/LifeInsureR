## ----echo = FALSE, message=FALSE----------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(knitr)
library(kableExtra)
library(LifeInsuranceContracts)
options(scipen=5)

library(pander)
## Modified pandoc.list function that also works with NULL entries in the lists:
pandoc.listRK.return <- function(elements, style = c('bullet', 'ordered', 'roman'), loose = FALSE, add.line.breaks = TRUE, add.end.of.list = TRUE, indent.level = 0, missing = panderOptions('missing')) { #nolint

    ## checks
    if (!is.logical(loose)) {
        stop('Wrong argument provided: loose')
    }

    ## default values
    if (missing(style)) {
        style <- panderOptions('list.style')
    } else {
        style <- match.arg(style)
    }

    ## replace missing values
    w <- which(is.na(elements))
    if (length(w) > 0) {
        elements[w] <- missing
    }

    ## helpers
    elements.l <- length(elements)
    marker     <- switch(style,
                         'bullet'  = rep('* ', elements.l),
                         'ordered' = paste0(1:elements.l, '. '),
                         'roman'   = paste0(as.roman(1:elements.l), '. '))

    ## number of elements should be more than one
    if (elements.l == 0) {
        return('')
    }

    ## recursive call
    i.lag <- 0
    res <- ifelse(add.line.breaks, '\n', '')
    nms = names(elements)
    for (i in 1:elements.l) {
        res <- paste0(res, paste(rep(' ', indent.level * 4), collapse = ''), marker[i - i.lag])
        if (nms[[i]] != "") {
            res <- paste0(res, nms[[i]], ': ')
        }

        if (length(elements[[i]]) <=1 && !is.list(elements[[i]])) {
            res <- paste0(res, elements[[i]], '\n')
        } else {
            i.lag <<- i.lag + 1
            res <- paste0(res, '\n', pandoc.listRK.return(elements[[i]], style, loose, FALSE, FALSE, indent.level + 1))
        }
        res <- paste0(res, ifelse(loose, '\n', ''))
    }

    # res <- paste(sapply(1:elements.l, function(i) {
    #     if (length(elements[[i]]) <= 1 && !is.list(elements[[i]])) {
    #         paste0(paste(rep(' ', indent.level * 4), collapse = ''), marker[i - i.lag], elements[[i]])
    #     } else {
    #         i.lag <<- i.lag + 1
    #         pandoc.listRK.return(elements[[i]], style, loose, FALSE, FALSE, indent.level + 1)
    #     }}),
    #     collapse = '\n', ifelse(loose, '\n', ''))

    ## closing tag
    if (add.end.of.list) {
        res <- paste0(res, ifelse(loose, '', '\n\n'), '<!-- end of list -->\n')
    }
    if (add.line.breaks) {
        res <- add.blank.lines(res)
    }

    return(res)

}

#' @export
pandoc.listRK <- function(...)
    cat(pandoc.listRK.return(...))





## ----SimpleExampleRiskTarif---------------------------------------------------
library(magrittr)
library(MortalityTables)
library(LifeInsuranceContracts)
mortalityTables.load("Austria_Census")

Tarif.L71U = InsuranceTarif$new(
    name = "L71-U",
    type = "wholelife",
    tarif = "DeathPlus - Short Term Life Insurance",
    desc = "Term Life insurance (10 years) with constant sum insured and regular premiums",
    policyPeriod = 10, premiumPeriod = 10,  # premiumPeriod not needed, defaults to maturity

    mortalityTable = mortalityTable.mixed(
      table1 = mort.AT.census.2011.male, weight1 = 0.65, 
      table2 = mort.AT.census.2011.female, weight2 = 0.35
    ),
    i = 0.005, 
    tax = 0.04, 
    costs = initializeCosts(alpha = 0.05, gamma = 0.01, gamma.paidUp = 0.01, unitcosts = 10),
    surrenderValueCalculation = function(surrenderReserve, params, values) { 
      surrenderReserve * 0.9 
    }
);

## ----SimpleExampleRiskContract------------------------------------------------
contract.L71U  = InsuranceContract$new(
  Tarif.L71U, 
  age = 35, 
  contractClosing = as.Date("2020-08-18"), 
  sumInsured = 100000);

## ----SimpleExampleRiskValues--------------------------------------------------
contract.L71U$Values$premiums %>% kable
contract.L71U$Values$reserves %>% kable(digits=2)

## ----SimpleExampleRiskCF------------------------------------------------------
contract.L71U$Values$cashFlows %>% kable()
contract.L71U$Values$cashFlowsCosts %>% kable()

## ----SimpleExampleRiskPV------------------------------------------------------
contract.L71U$Values$presentValues %>% kable()
contract.L71U$Values$presentValuesCosts %>% kable()
contract.L71U$Values$premiums %>% kable()

## ----SimpleExampleRiskReserves------------------------------------------------
contract.L71U$Values$reserves %>% kable(digits=2)

## ----SimpleExampleRiskPremiumComposition--------------------------------------
contract.L71U$Values$premiumComposition %>% kable(digits=2)

## ----SimpleExampleRiskConversion----------------------------------------------
contract.L71U = contract.L71U$premiumWaiver(t = 5)
contract.L71U$Values$reserves %>% kable(digits=2)

## ----SimpleExampleRiskPremiumGrid---------------------------------------------
contractGridPremium(
  axes = list(age = seq(20, 80, 5), policyPeriod = seq(5, 40, 5)),
  tarif = Tarif.L71U, 
  contractClosing = as.Date("2020-08-18"), 
  sumInsured = 100000
)

## ----SimpleExampleRiskPremiumGrid3D-------------------------------------------
contractGridPremium(
  axes = list(age = seq(20, 80, 10), policyPeriod = seq(10, 40, 10), sumInsured = c(10000, 50000, 100000)),
  tarif = Tarif.L71U, 
  contractClosing = as.Date("2020-08-18")
)

## ----SimpleExampleRiskPremiumGridLifeTables-----------------------------------
contractGridPremium(
  axes = list(mortalityTable = mort.AT.census["m", ], age = seq(20, 80, 10)),
  tarif = Tarif.L71U, 
  sumInsured = 100000,
  contractClosing = as.Date("2020-08-18")
)

## -----------------------------------------------------------------------------
str(InsuranceContract.ParameterDefaults)

## ---- results="asis"----------------------------------------------------------

# pandoc.listRK(InsuranceContract.ParameterDefaults)

## ----TarifDefinition----------------------------------------------------------
riskTarif = InsuranceTarif$new(
  name = "Example Risk Tarif", 
  
)




