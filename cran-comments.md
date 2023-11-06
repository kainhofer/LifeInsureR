## R CMD check results

0 errors | 0 warnings | 1 note

* Sunsetting the LifeInsuranceContracts packge in favor of LifeInsureR.
This version ensures a seamless transition path for existing code using the 
LifeInsuranceContracts package. It will automatically install the new LifeInsureR
package, load it on loading and in addition print out a warning (on loading) to
use the new name instead.

Since the LifeInsureR package will export the same names as the previous 
LifeInsuranceContracts package, almost all code will continue to work. The only 
exception is when (internal) functions are explicitly called like 
LifeInsuranceContracts:::internalFuntion(...)
