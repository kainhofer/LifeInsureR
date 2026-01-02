
# Version 1.0.2: XXXX XX, 2026
  * Add debug flag GUI with method LIC_debug_configure()
  * First steps for company-specific templating
  * License change to MIT license
  * Improved helper function expect_equal_abs for unit test cases
  * Some more helper functions for profit calculation
  * New flag/parameter profitAttributionAtExpiration
  * New flag/parameter surrenderPenaltyOnPremiumWaiver to indicate whether the surrender penalty is applied to premium waivers, too
  * Allow policyperiod=Inf -> The policy term will be cut at the maximum age of the life table
  * New flag/parameter hasPremiumWaiver and hasSurrender
  * New copy method, which creates deep copies of child blocks -> contract$copy() must be used rather than contract$clone()

# Version 1.0.1: June 10, 2025
  * New parameters:
    - `survivalBenefit`: Generalize survival benefit vectors (previously: unit CF 1 at end of contract)
    - `gammaInZillmer`: As a company-specific feature, include gamma costs (but not beta) in the Zillmer premium
  * Improve test case generation: Also generate code to export sample contract to Excel
  * Add feature to round intermediate value using a `RoundingHelper` class (stored in `Parameters$Hooks$Rounding`)
  * Fix: `InsuranceContract$addBlock` now ensures that unique IDs are used for existing rider contracts 
  

# Version 1.0.0: October 27, 2023
  * Renamed package from LifeInsuranceContracts to LifeInsureR

