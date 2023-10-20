#' @include XXXCOMPANYXXX_General.R

############################################################################~#
#  XXXCOMPANYXXX Gewinnplan 1                                             ####
#  Profits:                                                               ####
#    * Interest profit: (profit rate - guaranteed interest)*reserve       ####
#    * Terminal profit: once/twice the last interest profit assignment    ####
############################################################################~#


#' @export
XXXCOMPANYXXX.Gewinnplan1 = ProfitParticipation$new(
  name = "XXXCOMPANYXXX Gewinnplan 1, Version 1",
  profitComponents = c("interest", "terminal"),
  profitClass = "1",

  waitingPeriod = 3,

  guaranteedInterest = 0.03,
  interestProfitRate = pmax(XXXCOMPANYXXX.Gesamtverzinsung - 0.03, 0),
  totalInterest = XXXCOMPANYXXX.Gesamtverzinsung,
  getTerminalBonusRate = function(res, rates, params, values) {
    # Schlussgewinn (Vielfaches des letzten Zinsgewinns)
    # lfd. Prämie: LZ<20: 1x, LZ>=20: 2x
    if (params$ContractData$policyPeriod < 20) {
      1
    } else {
      2
    }
  },

  getInterestOnProfits = PP.rate.interestProfitPlusGuarantee,

  getInterestProfitBase = PP.base.contractualReserve,
  getTerminalBonusBase = PP.base.totalProfitAssignment,

  calculateSurvivalBenefit = function(profits, rates, ...) { profits[,"regularBonus"] * (1 + rates$guaranteedInterest + rates$interestProfitRate) + profits[,"terminalBonus"]},
  calculateDeathBenefitAccrued  = PP.benefit.Profit,
  calculateSurrenderBenefitAccrued = PP.benefit.Profit,
  calculatePremiumWaiverBenefitAccrued = PP.benefit.Profit,
  calculatePremiumWaiverBenefitTerminal = PP.benefit.None
);
