# This file was created from a template provided by the LifeInsuranceContracts
# package. It's purpose is to read in a set of contract data and calculate the
# corresponding reserves (e.g. to validate the official numbers in the financial
# statements).
#
# Steps:
#   1. Implement the corresponding products in the files in the R/ subdirectory.
#      Use the LifeInsuranceContracts documentation available at
#      https://cran.r-project.org/web/packages/LifeInsuranceContracts/vignettes/using-the-lifeinsurancecontracts-package.html
#   2. Install the package (using the "Install" button in RStudio's "Build" pane)
#   3. Set up the mapping of the columns of the contract data source to the package's arguments.
#      The columns of the input data can be directly mapped to named arguments in LifeInsuranceContract$new(..) calls.
#   4. If some columns need manual modifications (e.g. sex or frequencies
#      expressed with other values than the package expects), update the
#      VTmodify.* functions below correspondingly.
#   5. Update the column types in the readXXXCOMPANYXXXBestand(..) function. This helps
#      preventing errors, as these columns are always cast to the required type.
#   6. The calculate_contract(..) function might need to some adjustments /
#      modifications, in particular when modified contracts, premiums waivers,
#      additional tariffs / single-payment add-ons etc. are present.
#   7. Depending on which columns / data are available in the company-provided
#      contract data, the column modifications / calculations of other reserves,
#      etc. at the end of the calculate_portfolio(...) function might need to
#      be adjusted.
#   8. Update the `files` and `outfile` variables to point to the input files
#      ("Bestandsdaten") and the output file name
#   9. Call the calculate_portfolio function on the contract data set (potentially
#      filtered to some subsets to prevent performance issues)
#
#  Typically, a call to calculate a portfolio and store the results in a dedicated
#  (Excel) output file is:
#      results = NULL; results = calculate_portfolio(bestandinfos.all,
#           tarif = c("ProdName1", "ProdName2"), GV = c("123"), debug =TRUE)
#      results %>%
#           openxlsx::write.xlsx(outfile("Prods-1-2"), asTable = TRUE,
#                  overwrite = TRUE, creator = "Your Name",
#                  sheetName = "Vergleichsrechnung", tabColour = "#FFE600")
#      openXL(outfile("Prods-1-2"))
#
#
#
# General Overview of the calculation procedure
#   1. The contract data are read in from the file names provided in the `files`
#      list and stored in the data.frame called `bestandinfos.all`.
#        a. Each file is read using the function `readXXXCOMPANYXXXBestand`.
#        b. The `readXXXCOMPANYXXXBestand` function uses read_excel to read in the raw data,
#           then ensures the defined columns have the proper data type.
#        c. The columns are renamed according to the mapping in `colMapping`
#        d. All contracts are sorted by `Polizzennummer`
#        e. Additional modifications are done by the function `VTmodify.general`.
#        f. Further custom modifications can be manually added either in readXXXCOMPANYXXXBestand or in VTmodify.general
#   2. All contracts are calculated by a call to `calculate_portfolio`. The arguments
#      tarif and GV can be used to restrict the calculation only to certain
#      products and/or profit classes. Additionally, n_max can be used to
#      calculate only the first n_max contracts.
#      The `calculate_portfolio` function does its work with the following steps:
#        a. The portfolio data is filtered with the given tariff, GV, skip, n_max arguments
#        b. Only the relevant columns of the portfolio data are taken, some
#           sanity checks (sumInsured > 0, premiumFrequency >= 0) are applied.
#        c. Grouping happens by column SliceID. This allows multiple portfolio
#           data rows to be combined to one contract with several slices / sum
#           increases, which are calculated as one contract (see section "10.3
#           Dynamic Increases" of the LifeInsuranceContracts vignette). If each
#           slice / dynamic increase is supposed to be calculated individually
#           and independent from the main contract / other increases, then the
#           column mapped to the SliceID column needs to have a different value
#           for each portfolio data row. If SliceID uses contract numbers, all
#           dynamics, etc. belonging to the same contract number will be combined
#           and calculated using $addDynamics
#        d. Each contract (entries with distinct SliceID value) is calculated in
#           a loop using the by_slice function, which calls the calculate_contract
#           function for each contract.
#   3. The `calculate_contract` function calculates one individual contract, with
#      the individual columns of the portfolio data passed as named parameters
#      to the function.
#        a. A progress message is printed (if applicable)
#        b. All slices are arranged by date, with the slice starting first
#           assumed to be the main contract part.
#        c. For the main contract, an instance of the LifeInsuranceContract
#           (with the given tariff / product) is created and all values of the
#           contract are automatically calculated by the package by default.
#        d. All additional slices (e.g. dynamic increases) with the same SliceID
#           are added using the $addDynamics method of the LifeInsuranceContract
#           class. The slice start date and duration are adjusted correspondingly.
#        e. The reserves are extracted from the contract and stored in the final
#           data.frame
#        z. If debug=TRUE, a column is added to the resulting data.frame containing the R code to reproduce with full contract.
#    4. The calculate_portfolio combines the data.frames returned for each
#       contract's calculate_contract call into one large data frame, adds some
#       derived columns and returns the data frame as result of the calculations.
#
#
#
# COLUMN MAPPING
# --------------
# The following columns / named parameters are typically used by a LifeInsuranceTariff
# implementation or the concrete contract as a LifeInsuranceContract object. Most
# parameters are not mandatory.
# Additional arguments / columns are possible and will be preserved, even if
# they are not used by the contract.
#   * `Polizzennummer`
#   * `SliceID`
#   * `balanceSheetDate`
#   * `tarif`
#   * `GV`
#   * `i`
#   * `sex`
#   * `age`
#   * `contractClosing`
#   * `sliceDate`
#   * `policyPeriod`
#   * `premiumPeriod`

#   * `premiumFrequency`
#   * `annuityFrequency`
#   * `sumInsured`

# Columns used for comparison with the calculated values:
#   * `Bruttoprämie`
#   * `Sparprämie`
#   * `Risikoprämie`
#   * `Kostenprämie`
#   * `Bilanzreserve`
#   * `Gewinnreserve`
#   * `Prämienübertrag`

################################################################################


library(here)
library(lubridate)
library(readxl)
library(magrittr)
library(purrr)
library(purrrlyr)
library(openxlsx)
library(tictoc)
library(tidyverse)
library(LifeInsuranceContractsXXXCOMPANYXXX)
mortalityTables.load("Austria_*")



########################################################################m#
# Database structure definitions / mapping                            ####
########################################################################m#

colMapping = c(
    # Polizzen- und Tarifdefinition
    `Polizzennummer` = "PolNr",
    `SliceID` = "PolNr",          # Grouping will happen by SliceID
    `balanceSheetDate` = "Datum",
    `tarif` = "Produkt",          # Used to search for the LifeInsuranctTarif object
    `GV` = "Gewinnverband",

    # Parameter des Vertrags
    `i` = "Garantiezins",
    `sex` = "Geschlecht",
    `age` = "Eintrittsalter",     # Alternatively, birth can be given
    # `birthDate` = "GebDat",
    `contractClosing` = "Abschluss",
    `sliceDate` = "Abschluss",
    `policyPeriod` = "LZ",

    `premiumFrequency` = "ZW",
    `sumInsured` = "vs",
    `initialCapital` = "Anfangskapital",
    `costWaiver` = "Kostenverzicht",

    # Kontrollgrößen (lt. Geschäftsplan abgeleitet)

    `Bruttoprämie` = "BPr",
    `Sparprämie` = "SparPr",
    `Risikoprämie` = "RisikoPr",
    `Kostenprämie` = "KostenPr",

    # Deckungskapital am jeweiligen Jahrestag der Versicherung
    `DKt` = "VK",
    `DKt+1` = "VK1",
    `DKt+2` = "VK2",

    # Bilanzwerte
    `Bilanzreserve` = "BilRes",
    `Gewinnreserve` = "GewRes",
    `Bilanz-Verwaltungskostenreserve` = "VerwKostRes",
    `Netto-Bilanzreserve` = "NettoBilRes"#,
    # `Prämienübertrag` = "???"
)
colNamesRelevant = names(colMapping) %>% `[<-`(., . == "", NA_character_) %>% coalesce(colMapping)

Problempolizzen = c()



########################################################################m#
# Helper functions                                                    ####
########################################################################m#

cleanDate = function(dt) {
    if (is.POSIXct(dt) || is.POSIXlt(dt) || is.Date(dt) || is.logical(dt))
        as.Date(dt)
    else if (is.numeric(dt))
        as.Date(dt, origin = "1899-12-30")
    else if (is.character(dt)) {
        warning("date column was read in as string: ", dt)
        Reduce(c, map(dt, function(d) {
            # browser()
            if (is.character(d) && !is.na(d) && str_detect(d, "^[0-9]+$"))
                d = as.numeric(d)
            if (is.numeric(d))
                return(as.Date(d, origin = "1899-12-30"))
            as.Date(d)
        }))
    }
}



VTmodify.general = function(data) {
    #browser()

    data %>%
        separate(tarif, into = c("tarif", "Produkt"), sep = "[ ]+", extra = "merge", fill = "left") %>%
        mutate(
            sex = recode(
                as.character(sex),
                "M" = "male", "W" = "female", "F" = "female", "U" = "unisex",
                "m" = "male", "w" = "female", "f" = "female", "u" = "unisex",
                `0` = "male", `1` = "female", .default = "unisex"),
            premiumFrequency = recode(
                as.character(premiumFrequency),
                "J" = 1, "H" = 2, "V" = 4, "M" = 12, "E" = 0, .default = 1),
            policyPeriod = round(time_length(difftime(policyPeriod, contractClosing), "years")),

            # TODO: Apply further adjustments to the input data, e.g.
            # costWaiver = ifelse(costWaiver == "N", 0, 1),
            # sumRebate = -sumRebate,
            # sumInsured = if_else(VS == 0, Rentenhoehe, VS),

            # TODO: Rabatte, etc.

            id = paste0(tarif, "/GV", GV)
        )
}


########################################
readXXXCOMPANYXXXBestand = function(file, sheet = 1, ...) {
    charColumns = c(
        "tarif", "GV"
    )
    dateColumns = c(
        "balanceSheetDate",
        "contractClosing",
        "sliceDate",
        "birthData"
    );
    percentColumns = c(
        "i"
    )
    permilleColumns = c(

    )
    numericColumns = c(
        "policyPeriod", "age",
        "sumInsured", "initialCapital"
    );
    mapping = colMapping[names(colMapping) != ""]

    # browser()
    data = read_excel(file, sheet = sheet, ..., na = c("", "01.01.0100", "01.01.1000", "-328716", -328716)) %>%
        # Apply all renamings from colMapping:
        select(all_of(mapping), !all_of(mapping)) %>%
        mutate(
            across(any_of(dateColumns), cleanDate),
            across(any_of(numericColumns), as.numeric),
            across(any_of(charColumns), as.character),
            across(any_of(percentColumns), function(x) { as.numeric(x)/100 }),
            across(any_of(permilleColumns), function(x) { as.numeric(x)/1000 })
        ) %>%
        arrange(Polizzennummer) %>%   # Sortiere Verträge nach Polizzennummer
        VTmodify.general()
    data

}


###############################################################m#
## Definitionen: Kontrollrechnung der Deckungsrückstellung   ####
###############################################################m#

CountPol = 0
CountSlice = 0

calculate_contract = function(ctr.data, balanceSheetDate = as.Date("2023-12-31"), progress = 100, tariffs = XXXCOMPANYXXX.Tariffs, debug = FALSE, ...) {
    # browser()
    CountPol <<- CountPol + 1
    CountSlice <<- CountSlice + NROW(ctr.data)
    # browser()
    if (CountPol %% progress == 0) {
        print(paste0(CountPol, "/", CountSlice, ": Vertrag Nr. ", ctr.data[[1, "Polizzennummer"]], ", Tarif:", ctr.data[[1, "tarif"]], " GV", ctr.data[[1, "GV"]], ", ", NROW(ctr.data), " Scheiben"))
    }

    # Main part is the slice that
    #       a) starts first.
    # If two slices start at the same time, main part is
    #       b) the one with an initialCapital, or
    #       c) the one with the longest premium period (to exclude single payments from being the main part) or
    #       d) the highest sum insured
    ctr.data = ctr.data %>% arrange(sliceDate, desc(initialCapital), desc(premiumPeriod), desc(sumInsured))
    mainPart.orig = head(ctr.data, 1)
    mainPart.begin = mainPart.orig$sliceDate

    # Simplification: Adjust main part so  the contract end is always a full number of
    # years away (otherwise the reserves towards the end will be off considerably)
    #    if (!is.na(mainPart.orig$Vertragsende)) {
    #        mainPart.begin = mainPart.orig$sliceDate %>%
    #            `month<-`(month(mainPart.orig$Vertragsende))
    #    }
    # Changes in the current year might move the date after the balance sheet
    # date => as a workaround let the contract begin last year!
    while (mainPart.begin > balanceSheetDate) {
        year(mainPart.begin) = year(mainPart.begin) - 1
    }

    # If main part begins after contract, adjust the contract date to the main part's begin
    # Also recalculate the policy period from the (potentially changed) start and end dates of the main slice
    ctr.data = ctr.data %>%
        mutate(contractClosing = mainPart.begin)

    mainPart = head(ctr.data, 1) #%>%
    #        mutate(policyPeriod = year(Vertragsende) - year(contractClosing))
    dynamics = tail(ctr.data, -1)
    # browser()

    # Calculate main part
    args = as.list(mainPart)
    tarifName = args$tarif;
    tarifID = paste0(tarifName, "/", args$GV);
    args$tarif = XXXCOMPANYXXX.Tariff(tarifName, args$GV)

    if (is.null(args$tarif)) {
        warning("Unable to find Tarif for Product/GV", tarifID)
        return(
            data.frame(tarif = "?", mainPart.orig, Scheiben = NROW(ctr.data), date = balanceSheetDate, time = 0, Zillmer = 0, gamma = 0, `Balance Sheet Reserve` = 0, `unearned Premiums` = 0)
        )
    }

    ###################
    # Tariff-specific adjustments to the input data:
    #------------------
    # TODO: Implement all required tariff-specific adjustments here

    if (debug) {
        dbg.args = args;
        dbg.args$tarif = NULL;

        arguments = sapply(substitute(list(...))[-1], deparse);
        prepare.dbg.args = function(x) {
            if (is.function(x)) {paste(deparse(x), collapse = "\n\t\t")}
            else if (is.expression(x)) {as.character(x)}
            else if (is.na(x)) {x}
            else if (is.numeric(x)) {x}
            else if (is.character(x)) {deparse(as.character(x))}
            else if (is.Date(x)) {paste0("as.Date(\"", x, "\")")}
            else if (is.list(x)) {deparse(x)}
            else x
        }

        code = paste0(
            "InsuranceContract$new(\n\t",
            paste(
                c(
                    paste0("XXXCOMPANYXXX.Tariff(\"", tarifName, "\", ", dbg.args$GV, ")"), # ? und 117
                    paste(paste0("`", names(dbg.args), "`"), map(dbg.args, prepare.dbg.args), sep = " = ", collapse = ",\n\t"),
                    paste0("balanceSheetDate = as.Date(\"", balanceSheetDate, "\")"),
                    paste(names(arguments), arguments, sep = " = ", collapse = ",\n\t")
                ),
                collapse = ", \n\t"
            ),
            ")");
    }
    contract  = do.call(
        InsuranceContract$new,
        c(args, list(balanceSheetDate = balanceSheetDate, ...))
    )

    # Loop through all dynamics and call contract$addDynamics
    by_row(dynamics, function(dyn, ...) {
        # Vereinfachte Annahme: Dynamik ist immer am Jahrestag der Versicherung => Vergleiche nur Vertrags- mit Scheibenbeginnjahr
        # TODO: Einmal-Zuzahlungen müssen erkannt und anders gehandhabt werden!
        t = year(dyn$sliceDate) - year(dyn$contractClosing)
        contract$addDynamics(t = t, SumInsuredDelta = dyn$sumInsured)
        if (debug) {
            code <<- paste0(code, "$\naddDynamics(t = ", t, ", SumInsuredDelta = ", dyn$sumInsured, ")")
        }
    }, ...)


    Bilanzreserve = contract$Values$reservesBalanceSheet %>% `colnames<-`(c("Datum", "t", "Res.Netto.EY", "Res.Zillmer.EY", "Res.VwK.EY", "BilRes.EY", "PÜ.EY"))
    BilDRSt = Bilanzreserve %>% filter(Datum == balanceSheetDate)
    if (debug) {
        BilDRSt$code = code
    }
    t = which(Bilanzreserve$Datum == balanceSheetDate)

    data.frame(
        tarif = contract$tarif$name,
        mainPart.orig,
        Scheiben = NROW(ctr.data),

        # TODO: Weitere Vergleichswerte zurückliefern
        Premium.net.Cmp = contract$Values$premiumComposition[[t, "net"]],
        Premium.Zillmer.Cmp = contract$Values$premiumComposition[[t, "Zillmer"]],
        Premium.gross.Cmp = contract$Values$premiumComposition[[t, "gross"]],
        Premium.written.Cmp = contract$Values$premiumComposition[[t, "charged"]] - contract$Values$premiumComposition[[1, "tax"]],
        Premium.savings.Cmp = contract$Values$premiumComposition[[t, "savings"]],
        Premium.risk.Cmp = contract$Values$premiumComposition[[t, "risk"]],
        Premium.alphaZ.Cmp = contract$Values$premiumComposition[[t, "alpha.Zillmer"]],
        Premium.costs.Cmp = "TODO",
        Premium.gamma.Cmp = contract$Values$premiumComposition[[t, "gamma"]],
        Premium.sumRebate.Cmp = -contract$Values$premiumComposition[[t, "rebate.sum"]],

        DKt.Cmp = contract$Values$reserves[[t, "contractual"]],
        `DKt+1.Cmp` = if (t+1 > NROW(contract$Values$reserves)) 0 else contract$Values$reserves[[t+1, "contractual"]],
        `DKt+2.Cmp` = if (t+2 > NROW(contract$Values$reserves)) 0 else contract$Values$reserves[[t+2, "contractual"]],

        BilDRSt)
}


calculate_portfolio = function(bestand, tarif = NULL, GV = NULL, n_max = Inf, skip = 0, progress = 25, debug = FALSE, ...) {
    # browser()
    if (!missing(tarif) && !is.null(tarif)) {
        bestand = filter(bestand, tarif %in% !!tarif)
    }
    if (!missing(GV) && !is.null(GV)) {
        bestand = filter(bestand, GV %in% !!GV)
    }
    if (!missing(skip) && !is.null(skip) && skip > 0) {
        bestand = tail(bestand, -skip)
    }
    if (!missing(n_max) && !is.null(n_max) && n_max < Inf && n_max>0) {
        bestand = head(bestand, n_max)
    }

    input = bestand %>%
        select(all_of(colNamesRelevant)) %>%
        filter(sumInsured > 0, premiumFrequency >= 0) %>%  # <- Sanity Checks!
        # group_by(Polizzennummer)
        group_by(SliceID)

    CountPol <<- 0
    CountSlice <<- 0
    tic()
    # browser()
    Werte.berechnet = input %>%
        by_slice(calculate_contract, balanceSheetDate = ymd("2021-12-31"), .collate = "rows", progress = progress, debug = debug, ...)
    toc()

    Werte.berechnet %>%
        mutate(
            across(any_of(
                c("contractClosing", "sliceDate", "premiumWaiverDate", "date", "birthDate")
            ), as.Date, origin = "1970-01-01")) %>%
        mutate(
            # TODO: Implement comparisons to all values given in the XXXCOMPANYXXX portfolio file!
            #date = as.Date(date, origin = "1970-01-01"),
            BilRes.VU = bestand$Bilanzreserve, #`Bilanzreserve`, #+ `Bilanz.Verwaltungskostenreserve` ,#+ `Reserve.prämienfrei`,
            Fehlbetrag = BilRes.Cmp - BilRes.VU,
            `abs.FB` = abs(Fehlbetrag),
            `rel.FB` = Fehlbetrag / BilRes.Cmp,
            # `abs.FB NP` = abs(`Premium.net` - `Nettoprämie`),
            `abs.FB BP` = abs(`Premium.gross.Cmp` - `Bruttoprämie`),
            # `Monate verschoben` = (month(Vertragsende) != month(sliceDate)),
            exclude = c(""),
            Grund = c(""),
            Bilanzdaten = NULL
        )
}





###############################################################m#
## Bestände einlesen                                         ####
###############################################################m#

files = c(
    # here("Polizzeninfos", "2021-YE", "Einzelposten_2112.xlsx")
    here("Polizzeninfos", "2022-YE", "Einzelposten_2212.xlsx")
)
outfile = function(tarif) { here(paste0("XXXCOMPANYXXX_2022-YE_Vergleichsrechnung", paste(tarif, collapse = "_", sep= "_"), ".xlsx"))}

bestandinfos.all = bind_rows(map(unique(unname(unlist(files))), readXXXCOMPANYXXXBestand, skip = 0, guess_max = 99999, n_max = 99999))
bestandinfos.all %<>%
    filter(SATZTYP != "3A")

# Filter out all problematic contracts
bestandinfos = bestandinfos.all %>%
    filter(!Polizzennummer %in% Problempolizzen)



## PLAUSICHECKS auf Bestandsinfo:   ####

# Gesamtzahl verschiedene Polizzen
bestandinfos.all$Polizzennummer %>% unique %>% length
bestandinfos$Polizzennummer %>% unique %>% length

# 1) Schnelle übersicht über Anzahl Scheiben pro Vertrag
bestandinfos %>%
    group_by(Polizzennummer) %>%
    summarize(n = n()) %>%
    group_by(n) %>% summarize(Anzahl = n())
bestandinfos %>%
    group_by(Polizzennummer) %>%
    summarize(n = n()) %>%
    ggplot(aes(x = n)) + geom_histogram(binwidth = 1) +
    stat_bin(aes(y=..count.., label=..count..), geom= "text", vjust=-.5)

# 2) Überblick über Verträge, deren Scheiben alle nach Vertragsbeginn beginnen (Vertragsänderungen)


# 3) Prämienfreigestellte Verträge:
#     -) Scheiben, die ab Abschluss prämienfrei sind
#     -) Haben HV und alle Scheiben denselben Prf-Zeitpunkt?

# 4) Basic sanity checks: VS<0, keine Prämienzahlung
bestandinfos %>% filter(premiumFrequency < 1)
bestandinfos %>% filter(sumInsured <= 0)
bestandinfos %>% filter(sliceDate < contractClosing)


bestandinfos %>%
    group_by(Polizzennummer, GV) %>%
    summarize(Anzahl = n()) %>%
    filter(Anzahl>1)


##############################################################################m#
##
##  CALCULATION                                                             ####
##
##############################################################################m#


# Calculate Tarif1 and Tarif2 => Copy and adjust for each run / company-specific implementation

results = NULL;
results = calculate_portfolio(bestandinfos.all,
    tarif = c("Tarif1", "Tarif2"),
    #GV = c("108", "109", "111", "113", "115", "117"),
    progress = 1, n_max = 9999, debug =TRUE)
results %>%
    openxlsx::write.xlsx(outfile("Tarif-1-2"), asTable = TRUE, overwrite = TRUE, sheetName = "Vergleichsrechnung", tabColour = "#80FF8F")
openXL(outfile("Tarif-1-2"))



