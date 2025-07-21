##############################################################################m##
#     Tarif {{company}}.{{tarif}}
##############################################################################m##

# {{name}}, Tarif {{tarif}}
# Typ: TODO
# TODO, Garantiezins TODO%

#' @export
{{company}}.{{tarif}} = InsuranceTarif$new(
  name = "Tarif {{tarif}}",
  type = "{{type}}",
  tarif = "Tarif {{tarif}}",
  desc = "TODO",
  
  mortalityTable = NULL, # TODO
  i = 0.0, # TODO
  tax = 0.04,
  costs = initializeCosts(), # TODO

  premiumFrequencyLoading = {{company}}.UJZ,
  premiumRebate = {{company}}.sumRebate,
  surrenderValueCalculation = {{company}}.surrender#,
  
  #    profitParticipationScheme = TODO
) %>%
  {{company}}.register("{{tarif}}"); # , GV = "", AVB = "12");



